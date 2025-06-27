type t

val make :
     ?interleaved:bool
  -> ?minpoll:int
  -> ?maxpoll:int
  -> ?max_delay:float
  -> ?max_delay_ratio:float
  -> ?max_delay_dev_ratio:float
  -> ?presend_minpoll:int
  -> int
  -> t
