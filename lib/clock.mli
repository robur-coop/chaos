type on_slew =
  raw:Ptime.t -> cooked:Ptime.t -> dfreq:float -> doffset:float -> unit

val init : (unit -> int) -> unit
val register_on_slew : on_slew -> unit
val frequency : unit -> float

val read_raw_time : unit -> Ptime.t
(** Read the {b system} clock (without correction). *)

val read_cooked_time : unit -> Ptime.t
(** Read the {b system} clock, corrected according to all accumulated drifts and
    uncompensated offsets.

    Time calculation according to drift consists of a frequency, an offset, and
    the time between a {i base} and the given date by the system (called
    duration), where the correction applied is equal to:

    {[
      let now = system_clock () in
      let duration = now - base in
      let correction = (-1e-6 * freq * duration) - offset
      now + correction
    ]}

    The base used is updated every 1,000 seconds. At this interval, all
    corrections are compensated for in the offset value. *)

val adjust : Ptime.t -> float
(** [adjust v] is equivalent to the correction (see {!val:read_cooked_time})
    applied to the system time. *)

val precision_as_quantum : unit -> float
(** Routine to read the system precision in terms of the actual time step. *)

val accumulate_freq_and_offset : dfreq:float -> doffset:float -> float -> unit
(** Performe the combination of modifying the frequency and applying a slew, in
    one easy step. *)
