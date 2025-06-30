(* How to deals with the measurements and statistics of each of the source. *)

type t

val make :
     ?min_samples:int
  -> ?max_samples:int
  -> ?min_delay:float
  -> ?asymmetry:float
  -> int
  -> t
(** [make ref_id] creates a new instance of statistics handler. *)

val reset : t -> unit
(** This function resets an instance. *)

val accumulate : t -> Sample.t -> unit
(** [accumulate t sample] accumulates a single sample into the statistics
    handler. *)

val regression : Local.t -> t -> unit
(** [regression t] runs the linear regression operation on the data. It finds
    the set of most recent samples that give the tightest confidence interval
    for the frequency, and truncates the register down to that number of
    samples. *)

val get_frequency_range : t -> float * float
(** [get_frequency_range t] returns the assumed worst case range of values that
    this source's frequency lies within. Frequency is defined as the amount of
    time the local local gains relative to the source per unit local clock time.
*)

val get_delay_test_data :
  t -> Ptime.t -> (float * float * float * float * float) option
(** [get_delay_test_data] gets data needed for testing NTP delay. *)

val get_predict_offset : t -> Ptime.t -> float
