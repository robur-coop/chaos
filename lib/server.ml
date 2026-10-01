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

type counters = {
    mutable requests: int
  ; mutable responses: int
  ; mutable authenticated: int
  ; mutable kod: int
  ; mutable bad_auth: int
  ; mutable ignored: int
}

type t = { clients: (Ipaddr.t, bucket) Hashtbl.t; counters: counters }

let make () =
  let counters =
    {
      requests= 0
    ; responses= 0
    ; authenticated= 0
    ; kod= 0
    ; bad_auth= 0
    ; ignored= 0
    }
  in
  { clients= Hashtbl.create 0x100; counters }

let counters t = t.counters
let clients t = Hashtbl.length t.clients

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

let server_flags ~leap =
  (leap lsl 6) lor (4 lsl 3) lor 4 (* LI | NTPv4 | server *)

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
  t.counters.requests <- t.counters.requests + 1;
  match (Packet.flags_to_mode request.Packet.flags, auth) with
  | _, `Invalid ->
      t.counters.bad_auth <- t.counters.bad_auth + 1;
      Log.debug (fun m ->
          m "dropping request with bad authentication from %a" Ipaddr.pp peer);
      None
  | `Client, _ ->
      let sign = match auth with `Valid kid -> Some kid | _ -> None in
      let mono = Clock.read_raw_time () in
      if Option.is_some sign then
        t.counters.authenticated <- t.counters.authenticated + 1;
      if allow t peer mono then begin
        t.counters.responses <- t.counters.responses + 1;
        Some (reply ~reference ~rx request, sign)
      end
      else begin
        t.counters.kod <- t.counters.kod + 1;
        Log.debug (fun m -> m "rate-limited %a (KoD)" Ipaddr.pp peer);
        Some (kod_response request, sign)
      end
  | _ ->
      t.counters.ignored <- t.counters.ignored + 1;
      None
