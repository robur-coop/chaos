let src = Logs.Src.create "chaos.stats"

module Log = (val Logs.src_log src : Logs.LOG)

module Regress = struct
  external find_median : Float.Array.t -> (int[@untagged]) -> (float[@unboxed])
    = "unimplemented" "regress_find_median"
  [@@noalloc]

  external find_best_regression :
       runs_samples:(int[@untagged])
    -> n_samples:(int[@untagged])
    -> min_samples:(int[@untagged])
    -> times_back:Float.Array.t
    -> offsets:Float.Array.t
    -> weights:Float.Array.t
    -> est:Float.Array.t
    -> res:bytes
    -> (bool[@untagged]) = "unimplemented" "regress_find_best_regression"
  [@@noalloc]

  external multiple_regress :
       Float.Array.t
    -> Float.Array.t
    -> Float.Array.t
    -> (int[@untagged])
    -> bytes
    -> (bool[@untagged]) = "unimplemented" "regress_multiple_regress"
  [@@noalloc]
end

type t = {
    mutable ref_id: int
  ; source: Ipaddr.t * int
  ; min_samples: int (* User defined minimum and maximum number of samples *)
  ; max_samples: int
  ; fixed_min_delay: float (* User defined minimum delay *)
  ; fixed_asymmetry: float (* User defined asymmetry of network jitter *)
  ; mutable n_samples: int (* Number of samples currently stored *)
  ; mutable runs_samples: int
        (* Number of extra samples stored in [sample_times], [offsets] and
         [peer_delays]. *)
  ; mutable last_sample: int (* The index of the newest sample *)
  ; mutable regression_ok: bool
        (* Flag indicating whether last regression was successful *)
  ; mutable best_single_sample: int
        (* The best individual sample that we are holding, in terms of the minimum
         root distance at the present time. *)
  ; mutable min_delay_sample: int
        (* The index of the sample with minimum delay in [peer_delays] *)
  ; mutable estimated_offset: float
  ; mutable estimated_offset_sd: float
  ; mutable offset_time: Ptime.t
  ; mutable nruns: int
        (* Number of runs of the same sign amongst the residuals *)
  ; mutable asymmetry_run: int
        (* Number of consecutive estimated asymmetries with the same sign. The
         sign of the number encodes the sign of the asymmetry. *)
  ; mutable asymmetry: float
        (* This is the latest estimated asymmetry of network jitter. *)
  ; mutable estimated_frequency: float
  ; mutable estimated_frequency_sd: float
  ; mutable skew: float
        (* This is the assumed worst case bounds on the estimated frequency. We
         assume that the true frequency lies within +/- half this much about
         [estimated_frequency]. *)
  ; mutable std_dev: float
        (* This is the estimated standard deviation of the data points. *)
  ; sample_times: Ptime.t array
  ; offsets: Float.Array.t
  ; peer_delays: Float.Array.t
  ; peer_dispersions: Float.Array.t
  ; root_delays: Float.Array.t
  ; root_dispersions: Float.Array.t
}

let _MAX_SAMPLES = 64
let _REGRESS_RUNS_RATIO = 2
let _MIN_SKEW = 1e-12
let _MAX_SKEW = 1e2
let _MIN_STDDEV = 1e-9
let _WORST_CASE_FREQ_BOUND = 2000. /. 1e6
let _WORST_CASE_STDDEV_BOUND = 4.
let _SD_TO_DIST_RATIO = 0.7
let _MIN_ASYMMETRY = 0.45
let _MAX_ASYMMETRY = 0.5
let _MIN_ASYMMETRY_RUN = 10
let _MAX_ASYMMETRY_RUN = 1000

let source =
  Logs.Tag.def ~doc:"NTP source" "ntp.source" @@ fun ppf t ->
  let ipaddr, port = t.source in
  let uid = t.ref_id in
  Fmt.pf ppf "%a:%d:%04x" Ipaddr.pp ipaddr port uid

let make ?(min_samples = 1) ?(max_samples = _MAX_SAMPLES) ?(min_delay = 0.)
    ?(asymmetry = 1.) ?(ref_id = 0) (ipaddr, port) =
  let max_samples = Int.max (Int.min max_samples _MAX_SAMPLES) 1 in
  let min_samples = Int.max (Int.min min_samples max_samples) 1 in
  let sample_times =
    Array.make (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) Ptime.min
  in
  let offsets = Float.Array.create (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) in
  let peer_delays = Float.Array.create (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) in
  let peer_dispersions = Float.Array.create _MAX_SAMPLES in
  let root_delays = Float.Array.create _MAX_SAMPLES in
  let root_dispersions = Float.Array.create _MAX_SAMPLES in
  {
    ref_id
  ; source= (ipaddr, port)
  ; min_samples
  ; max_samples
  ; fixed_min_delay= min_delay
  ; fixed_asymmetry= asymmetry
  ; n_samples= 0
  ; runs_samples= 0
  ; last_sample= 0
  ; regression_ok= false
  ; best_single_sample= 0
  ; min_delay_sample= 0
  ; estimated_offset= 0.
  ; estimated_offset_sd= 0.
  ; offset_time= Ptime.min
  ; nruns= 0
  ; asymmetry_run= 0
  ; asymmetry= 0.
  ; estimated_frequency= 0.
  ; estimated_frequency_sd= 0.
  ; skew= 0.
  ; std_dev= 0.
  ; sample_times
  ; offsets
  ; peer_delays
  ; peer_dispersions
  ; root_delays
  ; root_dispersions
  }

let reset t =
  t.n_samples <- 0;
  t.runs_samples <- 0;
  t.last_sample <- 0;
  t.regression_ok <- false;
  t.best_single_sample <- 0;
  t.min_delay_sample <- 0;
  t.estimated_frequency <- 0.;
  t.estimated_frequency_sd <- _WORST_CASE_FREQ_BOUND;
  t.skew <- _WORST_CASE_FREQ_BOUND;
  t.estimated_offset <- 0.;
  t.estimated_offset_sd <- _WORST_CASE_STDDEV_BOUND;
  t.offset_time <- Ptime.min;
  t.std_dev <- _WORST_CASE_STDDEV_BOUND;
  t.nruns <- 0;
  t.asymmetry_run <- 0;
  t.asymmetry <- 0.

let set_ref_id t ~ref_id = t.ref_id <- ref_id

let get_buf_index t idx =
  (t.last_sample + (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) - t.n_samples + idx + 1)
  land 63

let get_runsbuf_index t idx =
  (t.last_sample
  + (2 * _MAX_SAMPLES * _REGRESS_RUNS_RATIO)
  - t.n_samples
  + idx
  + 1)
  land 127

let convert_to_intervals t ~off times_backs =
  let ts = t.sample_times.(t.last_sample) in
  for i = -t.runs_samples to t.n_samples - 1 do
    let diff = Ptime.diff t.sample_times.(get_runsbuf_index t i) ts in
    Float.Array.set times_backs (off + i) (Ptime.Span.to_float_s diff)
  done

let get_t_coef =
  let coefs =
    [|
       636.6; 31.6; 12.92; 8.61; 6.869; 5.959; 5.408; 5.041; 4.781; 4.587; 4.437
     ; 4.318; 4.221; 4.140; 4.073; 4.015; 3.965; 3.922; 3.883; 3.850; 3.819
     ; 3.792; 3.768; 3.745; 3.725; 3.707; 3.690; 3.674; 3.659; 3.646; 3.633
     ; 3.622; 3.611; 3.601; 3.591; 3.582; 3.574; 3.566; 3.558; 3.551
    |]
  in
  fun dof -> if dof <= 40 then coefs.(dof - 1) else 3.5

let clamp ~min:mi ~max:ma value = Float.max (Float.min value ma) mi

let min_round_trip_delay t =
  if t.fixed_min_delay > 0. then t.fixed_min_delay
  else if t.n_samples == 0 then Float.max_float
  else Float.Array.get t.peer_delays t.min_delay_sample

let estimate_asymmetry times_back offsets delays n asymmetry asymmetry_run =
  let a = Bytes.create 8 in
  if
    Regress.multiple_regress times_back delays offsets n a = false
    || begin
      let a = Int64.float_of_bits (Bytes.get_int64_ne a 0) in
      a *. float_of_int asymmetry_run < 0.
    end
  then `Stop (0.0, 0)
  else begin
    let a = Int64.float_of_bits (Bytes.get_int64_ne a 0) in
    let asymmetry_run =
      if a <= Float.neg _MIN_ASYMMETRY && asymmetry_run > -_MAX_ASYMMETRY_RUN
      then pred asymmetry_run
      else if a >= _MIN_ASYMMETRY && asymmetry_run < _MAX_ASYMMETRY_RUN then
        succ asymmetry_run
      else asymmetry_run
    in
    if abs asymmetry_run < _MIN_ASYMMETRY_RUN then
      `Stop (asymmetry, asymmetry_run)
    else
      `Continue
        ( clamp ~min:(Float.neg _MAX_ASYMMETRY) ~max:_MAX_ASYMMETRY a
        , asymmetry_run )
  end

let correct_asymmetry t times_back offsets =
  if t.fixed_asymmetry <> 0. then begin
    let delays = Float.Array.create (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) in
    let min_delay = min_round_trip_delay t in
    let n = t.runs_samples + t.n_samples in
    for i = 0 to n - 1 do
      let idx = get_runsbuf_index t (i - t.runs_samples) in
      Float.Array.set delays i (Float.Array.get t.peer_delays idx -. min_delay)
    done;
    if Float.abs t.fixed_asymmetry <= _MAX_ASYMMETRY then begin
      t.asymmetry <- t.fixed_asymmetry;
      for i = 0 to n - 1 do
        let v = Float.Array.get offsets i in
        Float.Array.set offsets i
          (v -. (t.asymmetry *. Float.Array.get delays i))
      done
    end
    else
      match
        estimate_asymmetry times_back offsets delays n t.asymmetry
          t.asymmetry_run
      with
      | `Continue (asymmetry, asymmetry_run) ->
          t.asymmetry <- asymmetry;
          t.asymmetry_run <- asymmetry_run;
          for i = 0 to n - 1 do
            let v = Float.Array.get offsets i in
            Float.Array.set offsets i
              (v -. (t.asymmetry *. Float.Array.get delays i))
          done
      | `Stop (asymmetry, asymmetry_run) ->
          t.asymmetry <- asymmetry;
          t.asymmetry_run <- asymmetry_run
  end

let find_best_sample_index t ~off times_back =
  (* With the value of skew that has been computed, see which of the samples
     offers the tightest bound on root distance. *)
  if t.n_samples > 0 then begin
    let best_root_distance = ref Float.max_float and best_index = ref 0 in
    for i = 0 to t.n_samples - 1 do
      let j = get_buf_index t i in
      let elapsed = Float.neg (Float.Array.get times_back (off + i)) in
      assert (elapsed >= 0.);
      let root_distance =
        Float.Array.get t.root_dispersions j
        +. (elapsed *. t.skew)
        +. (0.5 *. Float.Array.get t.root_delays j)
      in
      if root_distance < !best_root_distance then begin
        best_root_distance := root_distance;
        best_index := i
      end
    done;
    t.best_single_sample <- !best_index
  end

let find_min_delay_sample t =
  t.min_delay_sample <- get_runsbuf_index t (-t.runs_samples);
  for i = -t.runs_samples + 1 to t.n_samples - 1 do
    let index = get_runsbuf_index t i in
    if
      Float.Array.get t.peer_delays index
      < Float.Array.get t.peer_delays t.min_delay_sample
    then t.min_delay_sample <- index
  done

let prune t new_oldest =
  if new_oldest > 0 then begin
    assert (t.n_samples >= new_oldest);
    t.n_samples <- t.n_samples - new_oldest;
    t.runs_samples <- t.runs_samples + new_oldest;
    if t.runs_samples > t.n_samples * (_REGRESS_RUNS_RATIO - 1) then
      t.runs_samples <- t.n_samples * (_REGRESS_RUNS_RATIO - 1);
    assert (t.n_samples + t.runs_samples <= _MAX_SAMPLES * _REGRESS_RUNS_RATIO);
    find_min_delay_sample t
  end

(* This function runs the linear regression operation on the data. It finds the
   set of most recent samples that give the tightest confidence interval for the
   frequency, and truncates the register down to that number of samples. *)
let regression ?(tags = Logs.Tag.empty) t =
  let times_back = Float.Array.make (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) 0.0 in
  let offsets = Float.Array.make (_MAX_SAMPLES * _REGRESS_RUNS_RATIO) 0.0 in
  let peer_distances = Float.Array.make _MAX_SAMPLES 0.0 in
  let weights = Float.Array.make _MAX_SAMPLES 0.0 in
  let min_distance = ref Float.max_float in
  begin
    convert_to_intervals t ~off:t.runs_samples times_back;
    if t.n_samples > 0 then begin
      for i = -t.runs_samples to t.n_samples - 1 do
        let value = Float.Array.get t.offsets (get_runsbuf_index t i) in
        Float.Array.set offsets (i + t.runs_samples) value
      done;
      for i = 0 to t.n_samples - 1 do
        let j = get_buf_index t i in
        let value =
          (0.5 *. Float.Array.get t.peer_delays (get_runsbuf_index t i))
          +. Float.Array.get t.peer_dispersions j
        in
        Float.Array.set peer_distances i value;
        if Float.Array.get peer_distances i < !min_distance then
          min_distance := Float.Array.get peer_distances i
      done;
      let precision = Clock.precision_as_quantum () in
      let median_distance = Regress.find_median peer_distances t.n_samples in
      let sd = (median_distance -. !min_distance) /. 0.7 in
      let sd = clamp ~min:precision ~max:!min_distance sd in
      min_distance := !min_distance +. precision;
      let sd_weight = ref 1. in
      for i = 0 to t.n_samples - 1 do
        sd_weight := 1.;
        if Float.Array.get peer_distances i > !min_distance then begin
          sd_weight :=
            !sd_weight
            +. ((Float.Array.get peer_distances i -. !min_distance) /. sd)
        end;
        Float.Array.set weights i (!sd_weight *. !sd_weight)
      done;
      correct_asymmetry t times_back offsets
    end;
    let est = Float.Array.make 5 0.0 in
    let res = Bytes.create (4 * 3) in
    t.regression_ok <-
      Regress.find_best_regression ~runs_samples:t.runs_samples
        ~n_samples:t.n_samples ~min_samples:t.min_samples ~times_back ~offsets
        ~weights ~est ~res;
    let est_intercept = Float.Array.get est 0 in
    let est_slope = Float.Array.get est 1 in
    let est_var = Float.Array.get est 2 in
    let est_intercept_sd = Float.Array.get est 3 in
    let est_slope_sd = Float.Array.get est 4 in
    let best_start = Bytes.get_int32_ne res 0 |> Int32.to_int in
    let nruns = Bytes.get_int32_ne res 4 |> Int32.to_int in
    let degrees_of_freedom = Bytes.get_int32_ne res 8 |> Int32.to_int in
    if t.regression_ok then begin
      t.estimated_frequency <- est_slope;
      t.estimated_frequency_sd <-
        clamp ~min:_MIN_SKEW ~max:_MAX_SKEW est_slope_sd;
      t.skew <- est_slope_sd *. get_t_coef degrees_of_freedom;
      t.estimated_offset <- est_intercept;
      t.offset_time <- t.sample_times.(t.last_sample);
      t.estimated_offset_sd <- est_intercept_sd;
      t.std_dev <- Float.max _MIN_STDDEV (sqrt est_var);
      t.nruns <- nruns;
      t.skew <- clamp ~min:_MIN_SKEW ~max:_MAX_SKEW t.skew;
      Logs.debug (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "off=%e freq=%e skew=%e n=%d bs=%d runs=%d asym=%f arun=%d"
            t.estimated_offset t.estimated_frequency t.skew t.n_samples
            best_start t.nruns t.asymmetry t.asymmetry_run);
      let times_back_start = t.runs_samples + best_start in
      prune t best_start;
      find_best_sample_index t ~off:times_back_start times_back
    end
    else begin
      t.estimated_frequency_sd <- _WORST_CASE_FREQ_BOUND;
      t.skew <- _WORST_CASE_FREQ_BOUND;
      t.estimated_offset_sd <- _WORST_CASE_STDDEV_BOUND;
      t.std_dev <- _WORST_CASE_STDDEV_BOUND;
      t.nruns <- 0;
      if t.n_samples > 0 then begin
        t.estimated_offset <- Float.Array.get t.offsets t.last_sample;
        t.offset_time <- t.sample_times.(t.last_sample)
      end
      else begin
        t.estimated_offset <- 0.;
        t.offset_time <- Ptime.min
      end;
      find_best_sample_index t ~off:0 times_back
    end
  end

let accumulate ?(tags = Logs.Tag.empty) t sample =
  if
    t.n_samples > 0
    && (t.n_samples == _MAX_SAMPLES || t.n_samples == t.max_samples)
  then prune t 1;
  if
    t.n_samples >= 1
    && Ptime.compare t.sample_times.(t.last_sample) sample.Sample.time >= 0
  then begin
    Log.warn (fun m ->
        let tags = Logs.Tag.add source t tags in
        m ~tags "Out of order sample detected, discarding history for %d"
          t.ref_id);
    reset t
  end;
  let n = (t.last_sample + 1) land 127 in
  t.last_sample <- n;
  let m = n land 63 in
  (* NOTE: we have to negate offset in this call, it is here that the sense of offset is flipped. *)
  t.sample_times.(n) <- sample.Sample.time;
  Float.Array.set t.offsets n (Float.neg sample.Sample.offset);
  (* Float.Array.set t.orig_offsets m (Float.neg sample.Sample.offset); *)
  Float.Array.set t.peer_delays n sample.Sample.peer_delay;
  Float.Array.set t.peer_dispersions m sample.Sample.peer_dispersion;
  Float.Array.set t.root_delays m sample.Sample.root_delay;
  Float.Array.set t.root_dispersions m sample.Sample.root_dispersion;
  if Float.Array.get t.peer_delays n < t.fixed_min_delay then
    Float.Array.set t.peer_delays n
      ((2. *. t.fixed_min_delay) -. Float.Array.get t.peer_delays n);
  if
    t.n_samples = 0
    || Float.Array.get t.peer_delays n
       < Float.Array.get t.peer_delays t.min_delay_sample
  then t.min_delay_sample <- n;
  t.n_samples <- t.n_samples + 1

(* Return the assumed worst case range of values that this source's frequency
   lies within. Frequency is defined as the amount of time the local clock gains
   relative to the source per unit local clock time. *)
let get_frequency_range t =
  if t.skew >= _WORST_CASE_FREQ_BOUND then
    (Float.neg _WORST_CASE_FREQ_BOUND, _WORST_CASE_FREQ_BOUND)
  else
    let lo = t.estimated_frequency -. t.skew in
    let hi = t.estimated_frequency +. t.skew in
    (lo, hi)

let min_round_trip_delay t =
  if t.fixed_min_delay > 0. then t.fixed_min_delay
  else if t.n_samples == 0 then Float.max_float
  else Float.Array.get t.peer_delays t.min_delay_sample

let get_delay_test_data t sample_time =
  if t.n_samples < 6 then None
  else
    let last_sample_ago =
      Ptime.(Span.to_float_s (diff sample_time t.offset_time))
    in
    let predicted_offset =
      t.estimated_offset +. (last_sample_ago *. t.estimated_frequency)
    in
    let min_delay = min_round_trip_delay t in
    let skew = t.skew in
    let std_dev = t.std_dev in
    Some (last_sample_ago, predicted_offset, min_delay, skew, std_dev)

let _MIN_SAMPLES_FOR_REGRESS = 3

let get_predict_offset t w =
  if t.n_samples < _MIN_SAMPLES_FOR_REGRESS then
    if t.n_samples > 0 then Float.Array.get t.offsets t.last_sample else 0.0
  else
    let elapsed = Ptime.(diff w t.offset_time) in
    let elapsed = Ptime.Span.to_float_s elapsed in
    t.estimated_offset +. (elapsed *. t.estimated_frequency)

let samples t = t.n_samples

type info = {
    lo_limit: float
  ; hi_limit: float
  ; root_distance: float
  ; std_dev: float
  ; first_sample_ago: float
  ; last_sample_ago: float
}

let get_selection_data ?(tags = Logs.Tag.empty) t now =
  if t.n_samples <= 0 then None
  else if t.regression_ok then begin
    let i = get_runsbuf_index t t.best_single_sample in
    let j = get_buf_index t t.best_single_sample in
    let std_dev = t.std_dev in
    let sample_elapsed =
      Float.abs Ptime.(Span.to_float_s (diff now t.sample_times.(i)))
    in
    let offset =
      Float.Array.get t.offsets i +. (sample_elapsed *. t.estimated_frequency)
    in
    let root_distance =
      (0.5 *. Float.Array.get t.root_delays j)
      +. Float.Array.get t.root_dispersions j
      +. (sample_elapsed *. t.skew)
    in
    let offset_lo_limit = offset -. root_distance in
    let offset_hi_limit = offset +. root_distance in
    let i = get_runsbuf_index t 0 in
    let first_sample_ago =
      Ptime.(Span.to_float_s (diff now t.sample_times.(i)))
    in
    let i = get_runsbuf_index t (t.n_samples - 1) in
    let last_sample_ago =
      Ptime.(Span.to_float_s (diff now t.sample_times.(i)))
    in
    Logs.debug (fun m ->
        let tags = Logs.Tag.add source t tags in
        m ~tags "n=%d off=%f dist=%f sd=%f first_ago=%f last_ago=%f" t.n_samples
          offset root_distance std_dev first_sample_ago last_sample_ago);
    Some
      {
        lo_limit= offset_lo_limit
      ; hi_limit= offset_hi_limit
      ; root_distance
      ; std_dev
      ; first_sample_ago
      ; last_sample_ago
      }
  end
  else None

type data = {
    ref_time: Ptime.t
  ; offset: float
  ; offset_sd: float
  ; frequency: float
  ; frequency_sd: float
  ; skew: float
  ; root_delay: float
  ; root_dispersion: float
}

let get_tracking_data ?(tags = Logs.Tag.empty) t =
  if t.n_samples <= 0 then Fmt.invalid_arg "Stats.get_tracking_data";
  let i = get_runsbuf_index t t.best_single_sample in
  let j = get_buf_index t t.best_single_sample in
  let ref_time = t.offset_time in
  let offset = t.estimated_offset in
  let offset_sd = t.estimated_offset_sd in
  let frequency = t.estimated_frequency in
  let frequency_sd = t.estimated_frequency_sd in
  let skew = t.skew in
  let root_delay = Float.Array.get t.root_delays j in
  let elapsed_sample =
    Ptime.(Span.to_float_s (diff t.offset_time t.sample_times.(i)))
  in
  let root_dispersion =
    Float.Array.get t.root_dispersions j
    +. (t.skew *. elapsed_sample)
    +. offset_sd
  in
  Logs.debug (fun m ->
      let tags = Logs.Tag.add source t tags in
      m ~tags "n=%d off=%f offsd=%f freq=%e freqsd=%e skew=%e delay=%f disp=%f"
        t.n_samples offset offset_sd frequency frequency_sd skew root_delay
        root_dispersion);
  {
    ref_time
  ; offset
  ; offset_sd
  ; frequency
  ; frequency_sd
  ; skew
  ; root_delay
  ; root_dispersion
  }

let adjust old now dfreq doffset =
  let elapsed = Ptime.(Span.to_float_s (diff now old)) in
  let delta = (elapsed *. dfreq) -. doffset in
  let delta_span = Ptime.Span.of_float_s delta in
  let delta_span = Option.get delta_span in
  let result = Ptime.add_span old delta_span in
  let result = Option.get result in
  (result, delta)

let slew_samples t now dfreq doffset =
  if t.n_samples > 0 then begin
    for m = -t.runs_samples to t.n_samples - 1 do
      let i = get_runsbuf_index t m in
      let sample = t.sample_times.(i) in
      let new_sample, delta = adjust sample now dfreq doffset in
      t.sample_times.(i) <- new_sample;
      Float.Array.set t.offsets i (Float.Array.get t.offsets i +. delta)
    done;
    let offset_time, delta = adjust t.offset_time now dfreq doffset in
    t.offset_time <- offset_time;
    t.estimated_offset <- t.estimated_offset +. delta;
    t.estimated_frequency <- (t.estimated_frequency -. dfreq) /. (1. -. dfreq)
  end
