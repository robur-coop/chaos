[@@@warning "-32"]

let src = Logs.Src.create "chaos.clock"

module Log = (val Logs.src_log src : Logs.LOG)

let freq = ref 0.0
let offset = ref 0.0
let last = ref Ptime.min
let now = ref (Fun.const 0)
let precision_quantum = ref 0.0

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

let read_raw_time () =
  Log.debug (fun m -> m "read system clock");
  of_int_ns (!now ())

let pp = Ptime.pp_human ~frac_s:9 ()

let rec adjust raw =
  let duration = Ptime.(Span.to_float_s (diff raw !last)) in
  if duration > _MIN_UPDATE_INTERVAL then
    let () = update_offset () in
    Float.neg !offset
  else (-1e-6 *. !freq *. duration) -. !offset

and cook_time raw =
  let corr = adjust raw in
  let corr = Ptime.Span.of_float_s corr in
  let corr = Option.get corr in
  let cooked = Ptime.add_span raw corr in
  Option.get cooked

and read_cooked_time () = (Fun.compose cook_time read_raw_time) ()

and update_offset () =
  let now = read_raw_time () in
  let duration = Ptime.(Span.to_float_s (diff now !last)) in
  offset := !offset +. (1e-6 *. !freq *. duration);
  last := now;
  Log.debug (fun m -> m "system-clock %a" pp (read_raw_time ()));
  Log.debug (fun m -> m "cooked-clock %a" pp (read_cooked_time ()));
  Log.debug (fun m -> m "system-clock offset=%e freq=%f" !offset !freq)

let frequency () = !freq
let precision_as_quantum () = !precision_quantum

let set_frequency freq' =
  let () = update_offset () in
  freq := freq'

let accrue_offset offset' _corr_rate = offset := !offset +. offset'
let clamp ~min:mi ~max:ma value = Float.max (Float.min value ma) mi

let init ~now:fn =
  now := fn;
  last := read_raw_time ();
  precision_quantum := clamp ~min:1e-9 ~max:1. (measure_clock_precision ())

let accumulate_freq_and_offset ~dfreq ~doffset corr_rate =
  let raw = read_raw_time () in
  let _cooked = cook_time raw in
  let old_freq = !freq in
  update_offset ();
  freq := !freq +. (dfreq *. (1e6 -. old_freq));
  freq := clamp ~min:(-5e5) ~max:5e5 !freq;
  accrue_offset doffset corr_rate
