[@@@warning "-26-34-36"]

let src = Logs.Src.create "chaos.reference"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
    mutable are_we_synchronised: bool
  ; mutable our_stratum: int
  ; our_ref_id: int
  ; mutable our_ref_time: Ptime.t option
  ; mutable our_skew: float
  ; mutable our_residual_freq: float
  ; mutable our_root_delay: float
  ; mutable our_root_dispersion: float
  ; mutable our_offset_sd: float
  ; mutable our_frequency_sd: float
}

let make () =
  let our_ref_id = Mirage_crypto_rng.generate 2 in
  let our_ref_id = String.get_int16_be our_ref_id 0 in
  {
    are_we_synchronised= false
  ; our_root_dispersion= 1.0
  ; our_root_delay= 1.0
  ; our_skew= 1.0
  ; our_frequency_sd= 0.0
  ; our_residual_freq= 0.0
  ; our_offset_sd= 0.0
  ; our_stratum= 0
  ; our_ref_id
  ; our_ref_time= None
  }

let square x = x *. x
let clamp ~min:mi ~max:ma value = Float.max (Float.min value ma) mi

let clock_estimates t data =
  let open Stats in
  let measured_freq = data.frequency in
  let measured_skew = data.skew in
  if Float.abs measured_skew > 1e-3 then
    Log.warn (fun m -> m "skew %f too large to track" measured_skew);
  let gain =
    if Float.abs measured_skew > 1e-3 then 0.0
    else
      3.0
      *. square t.our_skew
      /. ((3.0 *. square t.our_skew) +. square measured_skew)
  in
  let gain = clamp ~min:0. ~max:1. gain in
  let estimated_freq = gain *. measured_freq in
  let residual_freq = measured_freq -. estimated_freq in
  let extra_skew =
    Float.sqrt
      ((square (Float.neg estimated_freq) *. (1.0 -. gain))
      +. (square (measured_freq -. estimated_freq) *. gain))
  in
  let estimated_skew =
    t.our_skew +. (gain *. (measured_skew -. t.our_skew)) +. extra_skew
  in
  (estimated_freq, residual_freq, estimated_skew)

let get_root_dispersion t now =
  match t.our_ref_time with
  | Some our_ref_time ->
      let diff = Ptime.(Span.to_float_s (diff now our_ref_time)) in
      t.our_root_dispersion
      +. Float.abs diff
         *. (t.our_skew +. Float.abs t.our_residual_freq +. 1e-6)
  | None -> 1.0

let update t ~stratum data =
  let open Stats in
  let now0 = Clock.read_raw_time () in
  let uncorr_off = Clock.adjust now0 in
  let uncorr_off = Ptime.Span.of_float_s uncorr_off in
  let uncorr_off = Option.get uncorr_off in
  let now1 = Ptime.add_span now0 uncorr_off in
  let now1 = Option.get now1 in
  let elapsed = Ptime.(Span.to_float_s (diff now1 data.ref_time)) in
  let offset = data.offset +. (elapsed *. data.frequency) in
  let freq, residual_freq, skew = clock_estimates t data in
  t.our_stratum <- Int.min 16 (succ stratum);
  t.our_ref_time <- Some data.ref_time;
  t.our_skew <- skew;
  t.our_residual_freq <- residual_freq;
  t.our_root_delay <- data.root_delay;
  t.our_root_dispersion <- data.root_dispersion;
  t.our_frequency_sd <- data.frequency_sd;
  t.our_offset_sd <- data.offset_sd;
  let corr_rate = 0.0 in
  (* TODO(dinosaure): [corr_rate] is useless for [Clock] but it can be interesting to calculate it. *)
  Clock.accumulate_freq_and_offset ~dfreq:freq ~doffset:offset corr_rate
