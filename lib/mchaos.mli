type waker

val waker : unit -> waker
val idle : waker -> unit
val interrupt : waker -> unit

type listeners

val listeners : unit -> listeners
val kill : listeners -> unit

val listen :
     Chaos.Auth.t
  -> Mnet.UDP.state
  -> waker
  -> listeners
  -> Chaos.Source.rx list
  -> Chaos.Source.rx list

val step :
     Mnet.UDP.state
  -> waker
  -> unit Miou.orphans
  -> Chaos.Source.rx list
  -> Chaos.Source.t
  -> [ `Continue | `Stop ] * Chaos.Source.rx list

(** {2 Client.} *)

type origin = [ `Ipaddr of Ipaddr.t | `Domain_name of [ `host ] Domain_name.t ]
type daemon

type state = private {
    reference: Chaos.Reference.t
  ; mutable source: Chaos.Source.t option
  ; server: origin * int
  ; started: Ptime.t
}

val client :
     Mnet_dns.t
  -> Mnet.UDP.state
  -> Chaos.Auth.t
  -> Chaos.Auth.key option
  -> origin * int
  -> daemon * state

val stop : daemon -> unit
