(** NTP server: answer client requests from the local reference state, with a
    per-client Kiss-o'-Death rate limit. *)

type t
(** Server state, holding the per-client rate-limit table. *)

val make : unit -> t

val handle :
     t
  -> Reference.t
  -> rx:Ptime.t
  -> peer:Ipaddr.t
  -> Packet.t
  -> Packet.t option
(** [handle t reference ~now ~rx ~peer ~request] builds the NTP response to
    [request] received from [peer] at receive time [rx] ([now] is used to age the
    root dispersion):

    - [None] if [request] is not a client-mode request;
    - a Kiss-o'-Death "RATE" packet if [peer] exceeds the rate limit;
    - otherwise a server response filled from [reference] (stratum, leap, ref id,
      reference/origin/receive timestamps, root delay and dispersion).

    The returned packet has no transmit timestamp: it must be serialised with
    {!val:Packet.encode_into}, which sets it at the actual send time. *)
