let src = Logs.Src.create "chaos.server"

module Log = (val Logs.src_log src : Logs.LOG)

(* Token-bucket rate limit per client: a client may send a burst of
   [_RATE_BURST] requests, then is limited to one request per [_RATE_PERIOD]
   seconds on average. Over the limit, the server answers with a Kiss-o'-Death
   "RATE" packet instead of a normal response. A basic KoD is the same size as
   the request (48 bytes), so it is not an amplification vector. *)
let _RATE_PERIOD = 1.0
let _RATE_BURST = 8.0
let _MAX_CLIENTS = 1024
let _KOD_RATE = 0x52415445 (* "RATE" *)

type bucket = { mutable tokens: float; mutable last: Ptime.t }
type t = { clients: (Ipaddr.t, bucket) Hashtbl.t }

let make () = { clients= Hashtbl.create 0x100 }

(* [allow t peer mono] consumes a token for [peer] at monotonic time [mono] and
   returns whether the request is within the rate limit. *)
let allow t peer mono =
  match Hashtbl.find_opt t.clients peer with
  | Some b ->
      let elapsed = Float.max 0. Ptime.(Span.to_float_s (diff mono b.last)) in
      b.tokens <- Float.min _RATE_BURST (b.tokens +. (elapsed /. _RATE_PERIOD));
      b.last <- mono;
      if b.tokens >= 1.0 then begin
        b.tokens <- b.tokens -. 1.0;
        true
      end
      else false
  | None ->
      (* Bound the table to avoid unbounded growth under a spoofed-source flood;
         on overflow we drop the whole limiter state (fail-open). *)
      if Hashtbl.length t.clients >= _MAX_CLIENTS then begin
        Log.debug (fun m -> m "rate-limit table full, clearing");
        Hashtbl.clear t.clients
      end;
      Hashtbl.replace t.clients peer { tokens= _RATE_BURST -. 1.0; last= mono };
      true

let server_flags ~leap = (leap lsl 6) lor (4 lsl 3) lor 4 (* LI | NTPv4 | server *)

let kod_response request =
  {
    Packet.flags= server_flags ~leap:3 (* LEAP_Unsynchronised *)
  ; stratum= 0 (* NTP_INVALID_STRATUM *)
  ; poll= request.Packet.poll
  ; precision= Clock.precision_as_log ()
  ; root_delay= 0.0
  ; root_dispersion= 0.0
  ; ref_id= _KOD_RATE
  ; ref_ts= None
  ; org_ts= request.Packet.tx_ts
  ; rx_ts= None
  ; tx_ts= None
  }

let reply ~reference ~rx request =
  let {
        Reference.synchronised= _
      ; leap
      ; stratum
      ; ref_id
      ; ref_time
      ; root_delay
      ; root_dispersion
      } =
    Reference.get_params reference rx
  in
  {
    Packet.flags= server_flags ~leap
  ; stratum
  ; poll= request.Packet.poll
  ; precision= Clock.precision_as_log ()
  ; root_delay
  ; root_dispersion
  ; ref_id
  ; ref_ts= Some ref_time
  ; org_ts= request.Packet.tx_ts (* originate = client's transmit timestamp *)
  ; rx_ts= Some rx (* our receive timestamp *)
  ; tx_ts= None (* transmit set by [Packet.encode_into] at send time *)
  }

let handle t reference ~auth ~rx ~peer request =
  match (Packet.flags_to_mode request.Packet.flags, auth) with
  | _, Auth.Invalid ->
      Log.debug (fun m ->
          m "dropping request with bad authentication from %a" Ipaddr.pp peer);
      None
  | `Client, _ ->
      let sign = match auth with Auth.Valid kid -> Some kid | _ -> None in
      let mono = Clock.read_raw_time () in
      if allow t peer mono then Some (reply ~reference ~rx request, sign)
      else begin
        Log.debug (fun m -> m "rate-limited %a (KoD)" Ipaddr.pp peer);
        Some (kod_response request, sign)
      end
  | _ -> None
