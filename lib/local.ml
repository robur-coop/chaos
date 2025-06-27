let src = Logs.Src.create "chaos.local"

module Log = (val Logs.src_log src : Logs.LOG)

type _ Effect.t += Monotonic : int Effect.t

type t = {
    mutable current_freq_ppm: float (* the current frequency, in ppm *)
  ; mutable max_freq_ppm: float (* Maximum allowed frequency, in ppm *)
  ; mutable temp_comp_ppm: float (* Temperature compensation, in ppm *)
  ; mutable precision_quantum: float
  ; mutable max_clock_error: float (* in ppm *)
}

let pp ppf t =
  Fmt.pf ppf
    "current_freq_ppm: %f, max_freq_ppm: %f, temp_comp_ppm: %f, \
     precision_quantum: %f, max_clock_error: %f"
    t.current_freq_ppm t.max_clock_error t.temp_comp_ppm t.precision_quantum
    t.max_clock_error

let measure_clock_precision () =
  let old_ts = ref (Effect.perform Monotonic) in
  let best = ref 1_000_000_000 in
  for _ = 0 to 99 do
    let ts = Effect.perform Monotonic in
    let diff = ts - !old_ts in
    old_ts := ts;
    if diff > 0 then if diff < !best then best := diff
  done;
  1e-9 *. Float.of_int !best

let clamp ~min:min_ ~max:max_ value = Float.max (Float.min value max_) min_

let make ?(max_freq_ppm = 500_000.0) () =
  let precision_quantum = measure_clock_precision () in
  let precision_quantum = clamp ~min:1e-9 ~max:1. precision_quantum in
  let precision_log = Float.(round (log precision_quantum /. log 2.)) in
  let precision_log = Float.to_int precision_log in
  assert (precision_log >= -30);
  Log.debug (fun m ->
      m "clock precision %.09f (%d)" precision_quantum precision_log);
  let max_clock_error = 1. *. 1e-6 in
  let current_freq_ppm = 0. in
  let temp_comp_ppm = 0. in
  {
    current_freq_ppm
  ; max_freq_ppm
  ; temp_comp_ppm
  ; precision_quantum
  ; max_clock_error
  }

let precision_as_quantum t = t.precision_quantum
let max_clock_error t = t.max_clock_error

let absolute_freq t =
  let freq = t.current_freq_ppm in
  if t.temp_comp_ppm <> 0. then
    (freq +. t.temp_comp_ppm) /. (1. -. (1e-6 *. t.temp_comp_ppm))
  else freq
