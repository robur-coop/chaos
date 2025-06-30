let src = Logs.Src.create "chaos.state"

module Log = (val Logs.src_log src : Logs.LOG)

type tx = Ptime.t Sched.Computation.t

type rx = {
    src: Ipaddr.V4.t
  ; port: int
  ; comp: (Ptime.t * Packet.t) Sched.Computation.t
}

type sleeper = Sched.Trigger.t

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
  ; stats : Stats.t
  ; local : Local.t
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
  let result = Sched.Computation.peek tx in
  let result = Option.get result in
  match (t.state, result) with
  | Invalid _, _ -> ()
  | End_of_round_trip, Error Discard ->
      Logs.debug ~src:t.src (fun m -> m "Roundtrip discarded by the receiver ")
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
      ignore (Sched.Computation.cancel rx Discard)
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

let log2_to_double l =
  let l = Int.max (Int.min l 31) (-31) in
  if l >= 0 then Float.of_int (1 lsl l) else 1. /. Float.of_int (1 lsl Int.abs l)

let _MAX_OFFSET = 4294967296.0
let _MIN_ENDOFTIME_DISTANCE = 365 * 24 * 3600
let _MAX_SERVER_INTERVAL = 4.0

let is_time_offset_sane ts offset =
  if offset >= Float.neg _MAX_OFFSET && offset < _MAX_OFFSET then begin
    let t = Ptime.to_float_s ts +. offset in
    t >= 0.0 && t < Float.of_int (0x7fffffff - _MIN_ENDOFTIME_DISTANCE)
    (* NOTE(dinosaure): we should check larger value like [1 << 32] as the
        maximum. *)
  end else false

let check_delay_ratio t sample_time delay =
  if 0.0 (* TODO(dinosaure): [t.max_delay_ratio] *) < 1. then true
  else
    match Stats.get_delay_test_data t.stats sample_time with
    | None -> true
    | Some (last_sample_ago, _predicted_offset, min_delay, skew, _std_dev) ->
        let max_delay =
          (min_delay *. 0.0 (* [t.max_delay_ratio] *))
          +. (last_sample_ago *. (skew +. Local.max_clock_error t.local))
        in
        delay <= max_delay

let check_delay_dev_ratio t sample_time offset delay =
  match Stats.get_delay_test_data t.stats sample_time with
  | None -> true
  | Some (last_sample_ago, predicted_offset, min_delay, skew, std_dev) ->
      (* Require that the ratio of the increase in delay from the minimum to the
         standard deviation is less than [max_delay_dev_ratio]. In the allowed
         increase in delay include also dispersion. *)
      let max_delta =
        (std_dev *. 10.0 (* TODO(dinosaure): [t.max_delay_dev_ratio] *))
        +. (last_sample_ago *. (skew +. Local.max_clock_error t.local))
      in
      let delta = (delay -. min_delay) /. 2. in
      if delta <= max_delta then true
      else
        let error_in_estimate = offset +. predicted_offset in
        Float.abs error_in_estimate -. delta > max_delta

let analyze t ~t1 ~t4 pkt =
  let remote_rx = Option.get pkt.Packet.rx_ts in
  let remote_tx = Option.get pkt.Packet.tx_ts in
  let remote_avg, remote_interval = average_and_diff ~earlier:remote_rx ~later:remote_tx in
  let local_rx = t4 in
  let local_tx = t1 in
  let local_avg, local_interval = average_and_diff ~earlier:local_rx ~later:local_rx in
  let root_delay = pkt.Packet.root_delay in
  let root_dispersion = pkt.Packet.root_dispersion in
  let response_time = Float.abs Ptime.(Span.to_float_s (diff remote_tx remote_rx)) in
  let precision = Local.precision_as_quantum t.local +. log2_to_double pkt.Packet.precision in
  let peer_delay = Float.abs Ptime.Span.(to_float_s (sub local_interval remote_interval)) in
  let peer_delay = if peer_delay < precision then precision else peer_delay in
  let offset = Ptime.(Span.to_float_s (diff remote_avg local_avg)) in
  let time = local_avg in
  let src_freq_lo, src_freq_hi = Stats.get_frequency_range t.stats in
  let skew = (src_freq_hi -. src_freq_lo) /. 2. in
  let peer_dispersion = precision +. (skew *. Float.abs (Ptime.Span.to_float_s local_interval)) in
  let sample =
    { Sample.time
    ; offset
    ; peer_delay
    ; peer_dispersion
    ; root_delay
    ; root_dispersion } in
  Logs.debug ~src:t.src (fun m -> m "%a" Sample.pp sample);
  let testA =
    sample.peer_delay -. sample.peer_dispersion <= 3.0 (* max delay *)
    && precision <= 3.0 (* max delay *)
    && is_time_offset_sane sample.time sample.offset
    && not (response_time > _MAX_SERVER_INTERVAL) in
  let testB = check_delay_ratio t sample.time sample.peer_delay in
  let testC = check_delay_dev_ratio t sample.time sample.offset sample.peer_delay in
  if testA && testB && testC then Some sample else None

let valid_nonce ~org ts =
  let str0 = Packet.ptime_to_string (Some org) in
  let str1 = Packet.ptime_to_string (Some ts) in
  String.equal str0 str1

let record_t4 _trigger t (rx, tx) =
  let result = Sched.Computation.peek rx in
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
          let osample = analyze t ~t1 ~t4 pkt in
          let fn sample =
            let estimated_offset = Stats.get_predict_offset t.stats sample.Sample.time in
            let error_in_estimate = Float.abs (Float.neg sample.offset -. estimated_offset) in
            Logs.debug ~src:t.src (fun m -> m "estimated offset: %f, error in estimate: %f" estimated_offset error_in_estimate); 
            Stats.accumulate t.stats sample;
            Stats.regression t.local t.stats in
          Option.iter fn osample;
          t.state <- End_of_round_trip
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
      (* NOTE(dinosaure): here, [Sched.Computation.cancel] will execute
         [record_t1] iff was not signaled by the user. By this way, we clean-up
         everything.
       *)
      ignore (Sched.Computation.cancel tx Discard)
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
        let send = Sched.Computation.create () in
        let ttx = Sched.Trigger.create () in
        let comp = Sched.Computation.create () in
        let trx = Sched.Trigger.create () in
        assert (Sched.Trigger.on_signal ttx t (send, comp) record_t1);
        assert (Sched.Trigger.on_signal trx t (comp, send) record_t4);
        assert (Sched.Computation.attach send ttx);
        assert (Sched.Computation.attach comp trx);
        let port = String.get_uint16_ne (Mirage_crypto_rng.generate 2) 0 in
        let recv = { src= t.dst; port; comp } in
        t.state <- New_round_trip { port; pkt; send; recv }
      end
      else Logs.warn ~src:t.src (fun m -> m "Unexpected trigger")
  | New_round_trip _ | Tx_sent _ | Rx_received _ | End_of_round_trip ->
      invalid_transition ~state:"new_round_trip" t

let handle t =
  Logs.debug ~src:t.src (fun m -> m "Update %a:%d" Ipaddr.V4.pp t.dst t.port);
  match t.state with
  | Invalid err -> `Error err
  | New_round_trip { port; pkt; send; recv } -> `Send (port, pkt, send, recv)
  | Sleep _ | Tx_sent _ | Rx_received _ -> `Await
  | End_of_round_trip ->
      let sleeper = Sched.Trigger.create () in
      let ns (* 1s *) = 1_000_000_000 in
      t.state <- Sleep { sleeper; ns };
      assert (Sched.Trigger.on_signal sleeper t () new_round_trip);
      `Sleep (sleeper, ns)

let make ?(port = 123) ~local dst =
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
  let send = Sched.Computation.create () in
  let ttx = Sched.Trigger.create () in
  let comp = Sched.Computation.create () in
  let trx = Sched.Trigger.create () in
  let src_port = String.get_uint16_ne (Mirage_crypto_rng.generate 2) 0 in
  let recv = { src= dst; port= src_port; comp } in
  let state = New_round_trip { port= src_port; pkt; send; recv } in
  let src = Logs.Src.create (Fmt.str "ntp:%a:%d" Ipaddr.V4.pp dst port) in
  let stats = Stats.make 0 in
  let t =
    {
      src
    ; dst
    ; port
    ; state
    ; poll= 6
    ; remote_poll= None
    ; number_of_roundtrips= 0
    ; stats
    ; local
    }
  in
  assert (Sched.Trigger.on_signal ttx t (send, comp) record_t1);
  assert (Sched.Trigger.on_signal trx t (comp, send) record_t4);
  assert (Sched.Computation.attach send ttx);
  assert (Sched.Computation.attach comp trx);
  t

let wake_up sleeper = Sched.Trigger.signal sleeper
let tx_sent tx ts = ignore (Sched.Computation.return tx ts)

let rx_received ~src ~src_port ~ts pkt (rx : rx) =
  if Ipaddr.V4.compare rx.src src == 0 && src_port == rx.port then
    ignore (Sched.Computation.return rx.comp (ts, pkt))

let rx_active rx = Sched.Computation.is_running rx.comp
let rx_port ({ port; _ } : rx) = port
let dst_unreachable tx = ignore (Sched.Computation.cancel tx Route_unreachable)
let rx_timeout rx = ignore (Sched.Computation.cancel rx.comp Timeout)
