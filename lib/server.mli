(** NTP server: answer client requests from the local reference state, with a
    per-client Kiss-o'-Death rate limit. *)

type t
(** Server state, holding the per-client rate-limit table. *)

val make : unit -> t

val handle :
     t
  -> Reference.t
  -> auth:Auth.check
  -> rx:Ptime.t
  -> peer:Ipaddr.t
  -> Packet.t
  -> (Packet.t * int option) option
(** [handle t reference ~auth ~rx ~peer request] builds the NTP response to
    [request] received from [peer] at receive time [rx], where [auth] is the
    verification result of the request's MAC:

    - [None] if the request is not a client-mode request, or carries a MAC that
      failed verification ([Auth.Invalid]);
    - otherwise [Some (packet, sign)] where [packet] is the response (a normal
      reply, or a Kiss-o'-Death "RATE" packet if the rate limit is exceeded) and
      [sign] is [Some key_id] when the response must be authenticated with that
      key (i.e. the request was authenticated), [None] otherwise.

    The returned packet has no transmit timestamp: it must be serialised with
    {!val:Packet.encode_into}, which sets it at the actual send time. *)
