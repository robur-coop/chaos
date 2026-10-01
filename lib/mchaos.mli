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
