val init : now:(unit -> int) -> unit

val read_raw_time : unit -> Ptime.t
(** Read the {b system} clock (without correction). *)

val read_cooked_time : unit -> Ptime.t
(** Read the {b system} clock, corrected according to all accumulated drifts and
    uncompensated offsets. *)

val adjust : Ptime.t -> float

val precision_as_quantum : unit -> float
(** Routine to read the system precision in terms of the actual time step. *)

val accumulate_freq_and_offset : dfreq:float -> doffset:float -> float -> unit
(** Performe the combination of modifying the frequency and applying a slew, in
    one easy step. *)
