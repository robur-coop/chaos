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

module Reachability : sig
  type t

  val make : unit -> t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

val make : ?port:int -> local:Local.t -> Ipaddr.V4.t -> t
val server : t -> Ipaddr.V4.t * int
val handle : t -> event
val stats : t -> Stats.t
val is_reachable : t -> bool
val reachability : t -> Reachability.t
val stratum : ?default:int -> t -> int

(*/*)

val wake_up : sleeper -> unit
val tx_sent : tx -> Ptime.t -> unit
val dst_unreachable : tx -> unit

val rx_received :
  src:Ipaddr.V4.t -> src_port:int -> ts:Ptime.t -> Packet.t -> rx -> unit

val rx_timeout : rx -> unit
val rx_port : rx -> int
val rx_active : rx -> bool
