external now : unit -> (int[@untagged]) = "unimplemented" "caml_utime_rdns"
[@@noalloc]

external init : (int[@untagged]) -> unit = "unimplemented" "caml_utime_init"
[@@noalloc]

external get_freq : unit -> (float[@unboxed])
  = "unimplemented" "caml_utime_get_freq"
[@@noalloc]

external set_freq : (float[@unboxed]) -> (float[@unboxed])
  = "unimplemented" "caml_utime_set_freq"
[@@noalloc]

let init ?(calibrate = 20_000_000) () = init calibrate
let nsec_per_day = 86_400 * 1_000_000_000
let ps_per_ns = 1_000L

let of_int_ns nsec =
  let days = nsec / nsec_per_day in
  let rem_ns = nsec mod nsec_per_day in
  let rem_ps = Int64.mul (Int64.of_int rem_ns) ps_per_ns in
  Ptime.v (days, rem_ps)

let ptime () = of_int_ns (now ())
