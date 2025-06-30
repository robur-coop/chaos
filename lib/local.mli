type t

val pp : t Fmt.t
val make : ?max_freq_ppm:float -> ?precision_quantum:float -> (unit -> int) -> t
val precision_as_quantum : t -> float
val max_clock_error : t -> float
val absolute_freq : t -> float
