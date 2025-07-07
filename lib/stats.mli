(* How to deals with the measurements and statistics of each of the source. *)

type t

val make :
     ?min_samples:int
  -> ?max_samples:int
  -> ?min_delay:float
  -> ?asymmetry:float
  -> ?ref_id:int
  -> Ipaddr.V4.t * int
  -> t
(** [make ref_id] creates a new instance of statistics handler. *)

val reset : t -> unit
(** This function resets an instance. *)

val set_ref_id : t -> ref_id:int -> unit

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
(** [get_predict_offset t when] predicts the offset of the local clock relative
    to a given source at a given local {i cooked} time. Positive indicates local
    clock is {b fast} relative to reference. *)

val samples : t -> int

type info = {
    lo_limit: float
  ; hi_limit: float
  ; root_distance: float
  ; std_dev: float
  ; first_sample_ago: float
  ; last_sample_ago: float
}
(** Structure used to hold info for selecting between sources. *)

val get_selection_data : t -> Ptime.t -> info option
(** Get data needed for selection *)

type data = {
    ref_time: Ptime.span
  ; average_offset: float
  ; offset_sd: float
  ; frequency: float
  ; frequency_sd: float
  ; skew: float
  ; root_delay: float
  ; root_dispersion: float
}

val get_tracking_data : t -> data
