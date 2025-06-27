type t
type tx
type rx
type sleeper

type event =
  [ `Send of int * Packet.t * tx * rx
  | `Await
  | `Sleep of sleeper * int
  | `Error of error ]

and error

val make : ?port:int -> Ipaddr.V4.t -> t
val server : t -> Ipaddr.V4.t * int
val handle : t -> event

(*/*)

val wake_up : sleeper -> unit
val tx_sent : tx -> Ptime.t -> unit
val dst_unreachable : tx -> unit

val rx_received :
  src:Ipaddr.V4.t -> src_port:int -> ts:Ptime.t -> Packet.t -> rx -> unit

val rx_timeout : rx -> unit
val rx_port : rx -> int
val rx_active : rx -> bool
