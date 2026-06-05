val select :
  Ptime.t -> Source.t list -> (Source.t * Stats.data * int * int) option
(** [select now sources] returns the reference source, the combined tracking
    data, the number of combined sources, and the voted leap indicator (NTP
    encoding), or [None] when there is nothing to update. *)
