type t
type tx
type rx
type sleeper

type event =
  [ `Send of int * Packet.t * tx * rx
  | `Await
  | `Sleep of sleeper * int
  | `Falseticker
  | `Server_unreachable ]

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

val is_dead : t -> bool
(** [is_dead t] is [true] once the source must be dropped (and, for a pool,
    replaced), for either of two reasons mirroring chrony: it has been
    unreachable (timeouts / unusable replies) for too many consecutive
    round-trips, or it has been a {e falseticker} (its interval disagrees with
    the majority, see {!set_falseticker}) for too many consecutive samples. *)

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

val set_falseticker : t -> bool -> unit
(** [set_falseticker t v] records the latest selection verdict for [t]: [v] is
    [true] when its interval disagrees with the majority. Called by {!Select};
    feeds the falseticker half of {!is_dead}. *)

(*/*)

val wake_up : sleeper -> unit
val tx_sent : tx -> Ptime.t -> unit
val dst_unreachable : tx -> unit

val rx_received :
     src:Ipaddr.t
  -> src_port:int
  -> ts:Ptime.t
  -> auth:Auth.result
  -> Packet.t
  -> rx
  -> unit

val rx_timeout : rx -> unit
val rx_port : rx -> int
val rx_active : rx -> bool
