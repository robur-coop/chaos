(** A (domain-safe) TSC-based clock. *)

external now : unit -> (int[@untagged]) = "unimplemented" "caml_utime_rdns"
[@@noalloc]

val init : ?calibrate:int -> unit -> unit
