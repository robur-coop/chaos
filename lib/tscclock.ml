external now : unit -> (int[@untagged]) = "unimplemented" "caml_utime_rdns"
[@@noalloc]

external init : (int[@untagged]) -> unit = "unimplemented" "caml_utime_init"
[@@noalloc]

let init ?(calibrate = 20_000_000) () = init calibrate
