let src = Logs.Src.create "chaos.state"

module Log = (val Logs.src_log src : Logs.LOG)
open Sched

type tx = Ptime.t Computation.t

type rx = {
    src: Ipaddr.V4.t
  ; port: int
  ; comp: (Ptime.t * Packet.t) Computation.t
}

type sleeper = Trigger.t

type event =
  [ `Send of int * Packet.t * tx * rx
  | `Await
  | `Sleep of sleeper * int
  | `Error of error ]

and state =
  | Sleep of { sleeper: sleeper; ns: int }
  | New_round_trip of { port: int; pkt: Packet.t; send: tx; recv: rx }
  | Tx_sent of { t1: Ptime.t }
  | Rx_received of { t4: Ptime.t; pkt: Packet.t }
  | End_of_round_trip
  | Invalid of error

and error = Server_unreachable

and t = {
    dst: Ipaddr.V4.t
  ; port: int
  ; mutable state: state
  ; src: Logs.Src.t
  ; poll: int (* Log2 defined polling interval *)
  ; mutable number_of_roundtrips: int
  ; mutable remote_poll: int option
        (* Log2 of server polling interval (recovered from received packets) *)
}

let pp_error ppf = function
  | Server_unreachable -> Fmt.string ppf "Server unreachable"

let pp_state ppf = function
  | Sleep { ns; _ } -> Fmt.pf ppf "Sleep:%dns" ns
  | New_round_trip _ -> Fmt.pf ppf "New_round_trip"
  | Tx_sent { t1; _ } ->
      Fmt.pf ppf "Tx_sent:%a" (Ptime.pp_human ~frac_s:9 ()) t1
  | Rx_received { t4; _ } ->
      Fmt.pf ppf "Rx_received:%a" (Ptime.pp_human ~frac_s:9 ()) t4
  | End_of_round_trip -> Fmt.pf ppf "End_of_round_trip"
  | Invalid err -> Fmt.pf ppf "%a" pp_error err

let pp ppf t = Fmt.pf ppf "%a" pp_state t.state
let server { dst; port; _ } = (dst, port)

exception Timeout
exception Discard
exception Route_unreachable

let[@inline never] invalid_transition ~state t =
  Logs.err ~src:t.src (fun m -> m "Invalid transition (%s): %a" state pp t);
  assert false

let record_t1 _trigger t (tx, rx) =
  let result = Computation.peek tx in
  let result = Option.get result in
  match (t.state, result) with
  | Invalid _, _ -> ()
  | (Sleep _ | Tx_sent _ | End_of_round_trip), _ ->
      invalid_transition ~state:"record_t1" t
  | New_round_trip _, Ok t1 -> t.state <- Tx_sent { t1 }
  | Rx_received { t4= _; pkt }, Ok _nonce ->
      t.remote_poll <- Some pkt.Packet.poll;
      t.number_of_roundtrips <- t.number_of_roundtrips + 1;
      t.state <- End_of_round_trip
  | _, Error Route_unreachable ->
      Logs.warn ~src:t.src (fun m -> m "Server unreachable");
      t.state <- Invalid Server_unreachable;
      (* NOTE(dinosaure): here, [Computation.cancel] will execute [record_t4]
         iff was not signaled by the user. By this way, we clean-up everything.
       *)
      ignore (Computation.cancel rx Discard)
  | _, Error Discard -> ()
  | _, Error exn ->
      Logs.err ~src:t.src (fun m ->
          m "Unexpected exception: %s" (Printexc.to_string exn))

let average_and_diff ~earlier ~later =
  let diff = Ptime.diff later earlier in
  let diff = Ptime.Span.to_float_s diff in
  let diff = diff /. 2. in
  (* NOTE(dinosaure): [of_float_s] fails only if we give an NaN value or
      something bigger than ~2'941'758 years... *)
  let diff = Option.get (Ptime.Span.of_float_s diff) in
  match Ptime.add_span earlier diff with
  | Some avg -> (avg, diff)
  | None ->
      Log.err (fun m ->
          m "Impossible to calculate the average between %a and %a" Ptime.pp
            earlier Ptime.pp later);
      assert false

[@@@warning "-26"]

let analyze t ~t1 ~t4 pkt =
  Logs.debug ~src:t.src (fun m ->
      m "dispersion: %f"
        ((pkt.Packet.root_delay /. 2.) +. pkt.Packet.root_dispersion));
  let remote_rx = Option.get pkt.Packet.rx_ts in
  let remote_tx = Option.get pkt.Packet.tx_ts in
  let remote_avg, remote_interval =
    average_and_diff ~earlier:remote_rx ~later:remote_tx
  in
  let local_rx = t4 in
  let local_tx = t1 in
  let local_avg, local_interval =
    average_and_diff ~earlier:local_rx ~later:local_rx
  in
  let root_delay = pkt.Packet.root_delay in
  let root_dispersion = pkt.Packet.root_dispersion in
  Logs.debug ~src:t.src (fun m -> m "t1: %a" (Ptime.pp_human ~frac_s:9 ()) t1);
  Logs.debug ~src:t.src (fun m -> m "t4: %a" (Ptime.pp_human ~frac_s:9 ()) t4);
  Logs.debug ~src:t.src (fun m ->
      m "remote_avg: %a" (Ptime.pp_human ~frac_s:9 ()) remote_avg);
  Logs.debug ~src:t.src (fun m ->
      m "remote_interval: %a" Ptime.Span.pp remote_interval);
  Logs.debug ~src:t.src (fun m ->
      m "local_avg: %a" (Ptime.pp_human ~frac_s:9 ()) local_avg);
  Logs.debug ~src:t.src (fun m ->
      m "local_interval: %a" Ptime.Span.pp local_interval);
  ()

let valid_nonce ~org ts =
  let str0 = Packet.ptime_to_string (Some org) in
  let str1 = Packet.ptime_to_string (Some ts) in
  String.equal str0 str1

let record_t4 _trigger t (rx, tx) =
  let result = Computation.peek rx in
  let result = Option.get result in
  match (t.state, result) with
  | Invalid _, _ -> ()
  | New_round_trip _, Ok (t4, pkt) ->
      t.remote_poll <- Some pkt.Packet.poll;
      t.state <- Rx_received { t4; pkt }
  | Tx_sent { t1 }, Ok (t4, pkt) -> begin
      match pkt.Packet.org_ts with
      | Some org when valid_nonce ~org t1 ->
          t.remote_poll <- Some pkt.Packet.poll;
          t.number_of_roundtrips <- t.number_of_roundtrips + 1;
          t.state <- End_of_round_trip;
          analyze t ~t1 ~t4 pkt
      | Some org_ts ->
          Logs.warn ~src:t.src (fun m ->
              m "Unexpected NTPv4 packet (org timestamp mismatches)");
          Logs.warn ~src:t.src (fun m ->
              m "org: %a" (Ptime.pp_human ~frac_s:9 ()) org_ts);
          Logs.warn ~src:t.src (fun m ->
              m "rxt: %a" (Ptime.pp_human ~frac_s:9 ()) t1);
          t.state <- End_of_round_trip
      | None ->
          Logs.warn ~src:t.src (fun m -> m "Unexpected NTPv4 packet");
          t.state <- End_of_round_trip
    end
  | (End_of_round_trip | Sleep _ | Rx_received _), _ ->
      invalid_transition ~state:"record_t4" t
  | _, Error Timeout ->
      t.state <- End_of_round_trip;
      Logs.warn ~src:t.src (fun m ->
          m "Server timeout (after %d roundtrip(s))" t.number_of_roundtrips);
      (* NOTE(dinosaure): here, [Computation.cancel] will execute [record_t1]
         iff was not signaled by the user. By this way, we clean-up everything.
       *)
      ignore (Computation.cancel tx Discard)
  | _, Error Discard -> ()
  | _, Error exn ->
      Logs.err ~src:t.src (fun m ->
          m "Unexpected exception: %s" (Printexc.to_string exn))

let new_round_trip trigger t () =
  Logs.debug ~src:t.src (fun m -> m "Start a new round trip");
  match t.state with
  | Invalid _ -> ()
  | Sleep { sleeper; ns= _ } ->
      if trigger == sleeper then begin
        let poll =
          match t.remote_poll with
          | Some remote_poll -> Int.max remote_poll t.poll
          | None -> t.poll
        in
        let pkt =
          {
            Packet.flags= 0x23 (* NTPv4, Client mode *)
          ; stratum= 0
          ; poll
          ; precision= 32 (* Don't reveal local time or state of the clock *)
          ; root_delay= 0.
          ; root_dispersion= 0.
          ; refid= 0l
          ; ref_ts= None
          ; org_ts= None
          ; rx_ts= None
          ; tx_ts= None
          }
        in
        let send = Computation.create () in
        let ttx = Trigger.create () in
        let comp = Computation.create () in
        let trx = Trigger.create () in
        assert (Trigger.on_signal ttx t (send, comp) record_t1);
        assert (Trigger.on_signal trx t (comp, send) record_t4);
        assert (Computation.attach send ttx);
        assert (Computation.attach comp trx);
        let port = String.get_uint16_ne (Mirage_crypto_rng.generate 2) 0 in
        let recv = { src= t.dst; port; comp } in
        t.state <- New_round_trip { port; pkt; send; recv }
      end
      else Logs.warn ~src:t.src (fun m -> m "Unexpected trigger")
  | New_round_trip _ | Tx_sent _ | Rx_received _ | End_of_round_trip ->
      invalid_transition ~state:"new_round_trip" t

let handle t =
  match t.state with
  | Invalid err -> `Error err
  | New_round_trip { port; pkt; send; recv } -> `Send (port, pkt, send, recv)
  | Sleep _ | Tx_sent _ | Rx_received _ -> `Await
  | End_of_round_trip ->
      let sleeper = Trigger.create () in
      let ns (* 1s *) = 1_000_000_000 in
      t.state <- Sleep { sleeper; ns };
      assert (Trigger.on_signal sleeper t () new_round_trip);
      `Sleep (sleeper, ns)

let make ?(port = 123) dst =
  let pkt =
    {
      Packet.flags= 0x23 (* NTPv4, Client mode *)
    ; stratum= 0
    ; poll= 0
    ; precision= 32 (* Don't reveal local time or state of the clock *)
    ; root_delay= 0.
    ; root_dispersion= 0.
    ; refid= 0l
    ; ref_ts= None
    ; org_ts= None
    ; rx_ts= None
    ; tx_ts= None
    }
  in
  let send = Computation.create () in
  let ttx = Trigger.create () in
  let comp = Computation.create () in
  let trx = Trigger.create () in
  let src_port = String.get_uint16_ne (Mirage_crypto_rng.generate 2) 0 in
  let recv = { src= dst; port= src_port; comp } in
  let state = New_round_trip { port= src_port; pkt; send; recv } in
  let src = Logs.Src.create (Fmt.str "ntp:%a:%d" Ipaddr.V4.pp dst port) in
  let t =
    {
      src
    ; dst
    ; port
    ; state
    ; poll= 6
    ; remote_poll= None
    ; number_of_roundtrips= 0
    }
  in
  assert (Trigger.on_signal ttx t (send, comp) record_t1);
  assert (Trigger.on_signal trx t (comp, send) record_t4);
  assert (Computation.attach send ttx);
  assert (Computation.attach comp trx);
  t

let wake_up sleeper = Trigger.signal sleeper
let tx_sent tx ts = ignore (Computation.return tx ts)

let rx_received ~src ~src_port ~ts pkt (rx : rx) =
  if Ipaddr.V4.compare rx.src src == 0 && src_port == rx.port then
    ignore (Computation.return rx.comp (ts, pkt))

let rx_active rx = Computation.is_running rx.comp
let rx_port ({ port; _ } : rx) = port
let dst_unreachable tx = ignore (Computation.cancel tx Route_unreachable)
let rx_timeout rx = ignore (Computation.cancel rx.comp Timeout)
