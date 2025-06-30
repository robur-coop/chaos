val get_freq : unit -> float
val set_freq : float -> float

(*/*)

external now : unit -> (int[@untagged]) = "unimplemented" "caml_utime_rdns"
[@@noalloc]

val init : ?calibrate:int -> unit -> unit
val ptime : unit -> Ptime.t
val of_int_ns : int -> Ptime.t
