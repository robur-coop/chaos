[@@@warning "-32"]

let src = Logs.Src.create "chaos.clock"

module Log = (val Logs.src_log src : Logs.LOG)

type on_slew =
  raw:Ptime.t -> cooked:Ptime.t -> dfreq:float -> doffset:float -> unit

let now = ref (Fun.const 0)
let freq = ref 0.0
let offset = ref 0.0
let last = ref Ptime.min
let precision_quantum = ref 0.0
let on_slew = Sequence.create ()
let register_on_slew fn = Sequence.(add Left) on_slew fn

let _MIN_UPDATE_INTERVAL =
  1000. (* Minimum interval between updates when frequency is constant. *)

let nsec_per_day = 86_400 * 1_000_000_000
and ps_per_ns = 1_000L

let of_int_ns nsec =
  let days = nsec / nsec_per_day in
  let rem_ns = nsec mod nsec_per_day in
  let rem_ps = Int64.mul (Int64.of_int rem_ns) ps_per_ns in
  Ptime.v (days, rem_ps)

let measure_clock_precision () =
  let old_ts = ref (!now ()) in
  let best = ref 1_000_000_000 in
  for _ = 0 to 99 do
    let ts = !now () in
    let diff = ts - !old_ts in
    old_ts := ts;
    if diff > 0 then if diff < !best then best := diff
  done;
  1e-9 *. Float.of_int !best

let read_raw_time () = of_int_ns (!now ())
let pp = Ptime.pp_human ~frac_s:9 ()

let read_raw_time () =
  let raw = read_raw_time () in
  Log.debug (fun m -> m "system-clock: %a" pp raw);
  raw

let update_offset () =
  let now = read_raw_time () in
  let duration = Ptime.(Span.to_float_s (diff now !last)) in
  offset := !offset +. (1e-6 *. !freq *. duration);
  last := now;
  Log.debug (fun m -> m "system-clock offset=%e freq=%f" !offset !freq)

let adjust raw =
  let duration = Ptime.(Span.to_float_s (diff raw !last)) in
  if duration > _MIN_UPDATE_INTERVAL then
    let () = update_offset () in
    Float.neg !offset
  else (-1e-6 *. !freq *. duration) -. !offset

(* The residual (not-yet-baked) part of the offset correction: the frequency
   drift accumulated since the last [update_offset]. This is the analogue of
   chrony's [LCL_GetOffsetCorrection] residual. The [offset] register holds the
   part already "applied" (cf. a kernel-slewed clock), so [cook_time] uses the
   sum [offset + pending] while the log reports only this small bounded part. *)
let pending_correction raw =
  let duration = Ptime.(Span.to_float_s (diff raw !last)) in
  -1e-6 *. !freq *. duration

let cook_time raw =
  let corr = adjust raw in
  let corr = Ptime.Span.of_float_s corr in
  let corr = Option.get corr in
  let cooked = Ptime.add_span raw corr in
  let cooked = Option.get cooked in
  Log.debug (fun m -> m "cooked-clock: %a" pp cooked);
  cooked

let read_cooked_time = Fun.compose cook_time read_raw_time
let frequency () = !freq
let precision_as_quantum () = !precision_quantum

let set_frequency freq' =
  let () = update_offset () in
  freq := freq'

let accrue_offset offset' _corr_rate = offset := !offset +. offset'
let clamp ~min:mi ~max:ma value = Float.max (Float.min value ma) mi

let init fn =
  now := fn;
  last := read_raw_time ();
  precision_quantum := clamp ~min:1e-9 ~max:1. (measure_clock_precision ())

let accumulate_freq_and_offset ~dfreq ~doffset corr_rate =
  let raw = read_raw_time () in
  (* Due to modifying the offset, this has to be the cooked time prior
     to the change we are about to make. *)
  let cooked = cook_time raw in
  let old_freq = !freq in
  (* TODO(dinosaure): check offset. *)
  update_offset ();
  (* Work out new absolute frequency. Note that absolute frequencies are handled
     in units of ppm, whereas the [dfreq] argument is in terms of the gradient of
     the (offset) v (local time) function. *)
  freq := !freq +. (dfreq *. (1e6 -. old_freq));
  freq := clamp ~min:(-5e5) ~max:5e5 !freq;
  Log.debug (fun m ->
      m "old_freq=%.3fppm new_freq=%.3fppm offset=%.6fsec" old_freq !freq
        doffset);
  accrue_offset doffset corr_rate;
  let fn on_slew = on_slew ~raw ~cooked ~dfreq ~doffset in
  Sequence.iter ~f:fn on_slew
