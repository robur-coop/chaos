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

val make : ?port:int -> ?key:Auth.key -> Ipaddr.t -> t
val server : t -> Ipaddr.t * int
val handle : ?tags:Logs.Tag.set -> t -> event
val stats : t -> Stats.t
val is_reachable : t -> bool
val reachability : t -> Reachability.t
val stratum : ?default:int -> t -> int

val key : t -> Auth.key option
(** Symmetric key used to authenticate exchanges with this source, if any. *)

val leap : t -> int
(** Leap indicator (NTP encoding: 0 normal, 1 insert, 2 delete) from the
    source's last synced packet. *)

(** {2 Selection state.}

    Persistent per-source state used by {!Select} to implement the reference
    source selection with hysteresis (cf. chrony's [sel_score]/[updates]). *)

val sel_score : t -> float
val set_sel_score : t -> float -> unit
val selected : t -> bool
val set_selected : t -> bool -> unit
val updates : t -> int
val set_updates : t -> int -> unit
val score_pending : t -> bool
val set_score_pending : t -> bool -> unit
val distant : t -> int
val set_distant : t -> int -> unit
val reachability_size : t -> int

(*/*)

val wake_up : sleeper -> unit
val tx_sent : tx -> Ptime.t -> unit
val dst_unreachable : tx -> unit

val rx_received :
     src:Ipaddr.t
  -> src_port:int
  -> ts:Ptime.t
  -> auth:Auth.check
  -> Packet.t
  -> rx
  -> unit

val rx_timeout : rx -> unit
val rx_port : rx -> int
val rx_active : rx -> bool
