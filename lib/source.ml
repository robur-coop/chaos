let _MAX_UNREACHABLE_RUN = 8
let _MAX_FALSETICKER_RUN = 8
let _MAX_OFFSET = 4294967296.0
let _MIN_ENDOFTIME_DISTANCE = 365 * 24 * 3600
let _MAX_SERVER_INTERVAL = 4.0
let _MAX_STRATUM = 16
let _INVALID_STRATUM = 0
let _MAX_DISPERSION = 16.0
let src = Logs.Src.create "chaos.state"

module Log = (val Logs.src_log src : Logs.LOG)

[@@@warning "-32"]

type tx = Ptime.t Sched.Computation.t

type rx = {
    src: Ipaddr.t
  ; port: int
  ; comp: (Ptime.t * Packet.t * Auth.result) Sched.Computation.t
}

type sleeper = Sched.Trigger.t

module Reachability = struct
  type t = { mutable reachability: int; mutable size: int }

  let make () = { reachability= 0; size= 0 }

  let int2bin ~len v =
    let buf = Bytes.create len in
    for i = len - 1 downto 0 do
      let chr = if v land (1 lsl i) != 0 then '1' else '0' in
      Bytes.set buf i chr
    done;
    Bytes.unsafe_to_string buf

  let pp ppf t = Fmt.pf ppf "%s/%d" (int2bin ~len:t.size t.reachability) t.size

  let update t reachable =
    t.reachability <- t.reachability lsl 1;
    t.reachability <- t.reachability lor Bool.to_int reachable;
    t.reachability <- t.reachability land ((1 lsl 8) - 1);
    if t.size < 8 then t.size <- t.size + 1

  let is_reachable t = t.reachability != 0
  let size t = t.size

  let compare t0 t1 =
    let value = t0.size - t1.size in
    if value == 0 then t0.reachability - t1.reachability else value
end

type event =
  [ `Send of int * Packet.t * tx * rx
  | `Await
  | `Sleep of sleeper * int
  | `Falseticker
  | `Server_unreachable ]

and state =
  | Sleep of { sleeper: sleeper; ns: int }
  | New_round_trip of { port: int; pkt: Packet.t; send: tx; recv: rx }
  | Tx_sent of { t1: Ptime.t }
  | Rx_received of { t4: Ptime.t; pkt: Packet.t; auth: Auth.result }
  | End_of_round_trip
  | Server_unreachable

and t = {
    dst: Ipaddr.t
  ; port: int
  ; mutable state: state
  ; mutable number_of_roundtrips: int
  ; mutable remote_poll: int option
        (* Log2 of server polling interval (recovered from received packets) *)
  ; stats: Stats.t
  ; mutable poll_score: float (* Score of current local poll *)
  ; mutable local_poll: int (* Log2 of polling interval at our end *)
  ; reachability: Reachability.t
  ; mutable unreachable_run: int
        (* Consecutive failed round-trips (timeouts / unusable replies); reset on
         a good sample. Used to declare a persistently unreachable source dead. *)
  ; mutable is_falseticker: bool
        (* Latest selection verdict: this source's interval disagrees with the
         majority (cf. chrony's SRC_FALSETICKER). Set by [Select]. *)
  ; mutable falseticker_run: int
        (* Consecutive usable samples taken while flagged a falseticker; reset on
         a truthful sample. Used to declare a persistent falseticker dead. *)
  ; key: Auth.key option
        (* Symmetric key used to authenticate exchanges with this source (the
         client signs its requests and requires authenticated responses). *)
  ; mutable stratum: int option
  ; mutable leap: int
        (* Leap indicator from the source's last synced packet (NTP encoding). *)
  ; mutable sel_score: float
        (* Persistent selection score, used for the hysteresis when choosing the
         reference source (cf. chrony's [sel_score]). *)
  ; mutable selected: bool
        (* Whether this source is the current reference (synchronisation)
         source. *)
  ; mutable updates: int
        (* Number of new samples accumulated since the last reference update. *)
  ; mutable score_pending: bool
        (* A new sample arrived whose effect on [sel_score] has not yet been
         applied by the selection. *)
  ; mutable distant: int
        (* Penalty counter excluding this source from the combination while
         positive (cf. chrony's [distant]). *)
}

let stats t = t.stats
let is_reachable t = Reachability.is_reachable t.reachability
let reachability t = t.reachability
let key t = t.key

let is_dead t =
  t.unreachable_run >= _MAX_UNREACHABLE_RUN
  || t.falseticker_run >= _MAX_FALSETICKER_RUN

let set_reachable t reachable =
  Reachability.update t.reachability reachable;
  if reachable then t.unreachable_run <- 0
  else t.unreachable_run <- succ t.unreachable_run

let leap t = t.leap
let sel_score t = t.sel_score
let set_sel_score t v = t.sel_score <- v
let selected t = t.selected
let set_selected t v = t.selected <- v
let updates t = t.updates
let set_updates t v = t.updates <- v
let score_pending t = t.score_pending
let set_score_pending t v = t.score_pending <- v
let distant t = t.distant
let set_distant t v = t.distant <- v
let set_falseticker t v = t.is_falseticker <- v
let reachability_size t = Reachability.size t.reachability

let source =
  Logs.Tag.def ~doc:"NTP source" "ntp.source" @@ fun ppf t ->
  Fmt.pf ppf "%a:%d" Ipaddr.pp t.dst t.port

let pp_state ppf = function
  | Sleep { ns; _ } -> Fmt.pf ppf "Sleep:%dns" ns
  | New_round_trip _ -> Fmt.pf ppf "New_round_trip"
  | Tx_sent { t1; _ } ->
      Fmt.pf ppf "Tx_sent:%a" (Ptime.pp_human ~frac_s:9 ()) t1
  | Rx_received { t4; _ } ->
      Fmt.pf ppf "Rx_received:%a" (Ptime.pp_human ~frac_s:9 ()) t4
  | End_of_round_trip -> Fmt.pf ppf "End_of_round_trip"
  | Server_unreachable -> Fmt.string ppf "Server_unreachable"

let pp ppf t = Fmt.pf ppf "%a" pp_state t.state
let server { dst; port; _ } = (dst, port)

exception Timeout
exception Discard
exception Route_unreachable

let[@inline never] invalid_transition ?(tags = Logs.Tag.empty) ~state t =
  Logs.err (fun m ->
      let tags = Logs.Tag.add source t tags in
      m ~tags "Invalid transition (%s): %a" state pp t);
  assert false

let log2_to_double l =
  let l = Int.max (Int.min l 31) (-31) in
  if l >= 0 then Float.of_int (1 lsl l) else 1. /. Float.of_int (1 lsl Int.abs l)

let average_and_diff ~earlier ~later =
  (* Like chrony's [UTI_AverageDiffTimespecs]: [diff] is the FULL interval
     [later - earlier] and the average is [earlier + diff / 2]. (Returning the
     halved interval would halve the round-trip [peer_delay] in [to_sample].) *)
  let diff = Ptime.diff later earlier in
  (* NOTE(dinosaure): [of_float_s] fails only if we give an NaN value or
      something bigger than ~2'941'758 years... *)
  let half =
    Option.get (Ptime.Span.of_float_s (Ptime.Span.to_float_s diff /. 2.))
  in
  match Ptime.add_span earlier half with
  | Some avg -> (avg, diff)
  | None ->
      Log.err (fun m ->
          m "Impossible to calculate the average between %a and %a" Ptime.pp
            earlier Ptime.pp later);
      assert false

let is_time_offset_sane ts offset =
  if offset >= Float.neg _MAX_OFFSET && offset < _MAX_OFFSET then begin
    let t = Ptime.to_float_s ts +. offset in
    (* Accept any time within the NTP era window [1970, 2106) (cf. chrony's
       HAVE_LONG_TIME_T branch with NTP_ERA_SPLIT = 0). The old 32-bit-time_t
       cap near 2037 would have rejected every sample past that date. *)
    t >= 0.0 && t < _MAX_OFFSET
  end
  else false

let check_delay_ratio t sample_time delay =
  if 0.0 (* TODO(dinosaure): [t.max_delay_ratio] *) < 1. then true
  else
    match Stats.get_delay_test_data t.stats sample_time with
    | None -> true
    | Some (last_sample_ago, _predicted_offset, min_delay, skew, _std_dev) ->
        let max_delay =
          (min_delay *. 0.0 (* [t.max_delay_ratio] *))
          +. (last_sample_ago *. (skew +. 1e-6))
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
        +. (last_sample_ago *. (skew +. 1e-6))
      in
      let delta = (delay -. min_delay) /. 2. in
      if delta <= max_delta then true
      else
        let error_in_estimate = offset +. predicted_offset in
        Float.abs error_in_estimate -. delta > max_delta

let valid_nonce ~org ts =
  let str0 = Packet.ptime_to_string (Some org) in
  let str1 = Packet.ptime_to_string (Some ts) in
  String.equal str0 str1

let valid_packet ~t1 pkt =
  match pkt.Packet.org_ts with
  | None -> false
  | Some org ->
      valid_nonce ~org t1
      && Option.is_some pkt.rx_ts
      && Option.is_some pkt.tx_ts

let synced_packet pkt =
  (* NOTE(dinosaure): the leap indicator is encoded in the 2 most significant
     bits of [flags] (the NTP lvm byte). chrony's test6 rejects a server whose
     leap indicator is LEAP_Unsynchronised (3). We must shift right, not left. *)
  (pkt.Packet.flags lsr 6) land 0x3 != 3
  && pkt.stratum < _MAX_STRATUM
  && pkt.stratum != _INVALID_STRATUM
  && (pkt.Packet.root_delay /. 2.0) +. pkt.Packet.root_dispersion
     < _MAX_DISPERSION

let get_poll_adjusted t error_in_estimate peer_distance =
  if error_in_estimate > peer_distance then
    Float.(neg (log (error_in_estimate /. peer_distance))) /. Float.log 2.0
  else
    let samples = Stats.samples t.stats in
    let poll_adj = ((Float.of_int samples /. 8.) -. 1.) /. 8. in
    if samples < 8 then poll_adj *. 2. else poll_adj

let adjust_poll t adj =
  t.poll_score <- t.poll_score +. adj;
  (* chrony truncates toward zero with [(int)] for BOTH [local_poll] and the
     [poll_score] adjustment; use the same integer for both to keep [poll_score]
     in [0, 1). *)
  if t.poll_score >= 1.0 then begin
    let n = Float.to_int t.poll_score in
    t.local_poll <- t.local_poll + n;
    t.poll_score <- t.poll_score -. Float.of_int n
  end;
  if t.poll_score < 0.0 then begin
    let n = Float.to_int (t.poll_score -. 1.) in
    t.local_poll <- t.local_poll + n;
    t.poll_score <- t.poll_score -. Float.of_int n
  end;
  (* Clamp polling interval to defined range [MINPOLL:6;MAXPOLL:10]. *)
  if t.local_poll < 6 then begin
    t.local_poll <- 6;
    t.poll_score <- 0.
  end
  else if t.local_poll > 10 then begin
    t.local_poll <- 10;
    t.poll_score <- 1.
  end

(* TODO(dinosaure): chrony attempts to be truly synchronised with the server,
   and calculations can "shift" the time when we should send the next packet (to
   initiate the next round trip). Thus, some time has elapsed between sending
   our packet for this round trip and receiving the packet from the server.
   chrony then attempts to subtract the delay of the current round trip from the
   wait time for the next round trip so that between sending the packet for the
   first round trip and sending the packet for the second round trip, the time
   is very close to the poll announced by the server.

   In our case, we need to identify where we end the round trip (from
   [record_t1] or [record_t4]). In the first case, we are "synchronous". In the
   second case, there is a delay (even so small) between [t1] and the moment
   when we want to initiate a new round trip ([now]). In this case, we need to
   calculate this delay and subtract it from the delay resulting from the poll
   announced by the server. *)
(* The transmit interval is our adaptive local poll (already clamped to
   [minpoll, maxpoll] by [adjust_poll]). Like chrony, we use our own poll and do
   not cap it at the server's announced poll. *)
let get_transmit_poll t = t.local_poll

let get_transmit_delay ?(tags = Logs.Tag.empty) t =
  let poll_to_use = get_transmit_poll t in
  Logs.debug (fun m ->
      let tags = Logs.Tag.add source t tags in
      m ~tags "poll-to-use: %d" poll_to_use);
  log2_to_double poll_to_use

let stratum ?(default = 0) t =
  match t.stratum with Some stratum -> stratum | None -> default

let to_sample t (t1, t4) pkt =
  let remote_rx = Option.get pkt.Packet.rx_ts in
  let remote_tx = Option.get pkt.Packet.tx_ts in
  let remote_avg, remote_interval =
    average_and_diff ~earlier:remote_rx ~later:remote_tx
  in
  let local_rx = t4 in
  let local_tx = t1 in
  let local_avg, local_interval =
    average_and_diff ~earlier:local_tx ~later:local_rx
  in
  let pkt_root_delay = pkt.Packet.root_delay in
  let pkt_root_dispersion = pkt.Packet.root_dispersion in
  let response_time =
    Float.abs Ptime.(Span.to_float_s (diff remote_tx remote_rx))
  in
  let precision =
    Clock.precision_as_quantum () +. log2_to_double pkt.Packet.precision
  in
  let peer_delay =
    Float.abs Ptime.Span.(to_float_s (sub local_interval remote_interval))
  in
  let peer_delay = if peer_delay < precision then precision else peer_delay in
  let offset = Ptime.(Span.to_float_s (diff remote_avg local_avg)) in
  let time = local_avg in
  let src_freq_lo, src_freq_hi = Stats.get_frequency_range t.stats in
  let skew = (src_freq_hi -. src_freq_lo) /. 2. in
  let peer_dispersion =
    precision +. (skew *. Float.abs (Ptime.Span.to_float_s local_interval))
  in
  (* NOTE(dinosaure): like chrony (ntp_core.c), the sample's root delay and root
     dispersion must include the peer delay and peer dispersion respectively. *)
  let root_delay = pkt_root_delay +. peer_delay in
  let root_dispersion = pkt_root_dispersion +. peer_dispersion in
  let m =
    {
      Sample.time
    ; offset
    ; peer_delay
    ; peer_dispersion
    ; root_delay
    ; root_dispersion
    }
  in
  (* Test A combines multiple tests to avoid changing the measurements log
     format and ntpdata report. It requires that the minimum estimate of the
     peer delay is not larger than the configured maximum (3.0), it is not a
     response in the "warm-up" exchange (NOTE(dinosaure): we don't do that and
     it's currently different from the burst mode), the configured offset
     correction is within the supported NTP interval and the server processing
     time is sane.

     chrony performs other checks when we are not in client/server mode (but in
     peer mode) and when we are managing an "interleaved" exchange. *)
  let testA =
    m.peer_delay -. m.peer_dispersion <= 3.0 (* max delay *)
    && precision <= 3.0 (* max delay *)
    && is_time_offset_sane m.time m.offset
    && not (response_time > _MAX_SERVER_INTERVAL)
  in
  (* Test B requires in client thaat the ratio of the round trip delay to the
     the minimum one currently in the stats data register is less than an
     administrator-defined value (NOTE(dinosaure): we currently can not define
     this value, this test should always be [true]). *)
  let testB = check_delay_ratio t m.time m.peer_delay in
  let testC = check_delay_dev_ratio t m.time m.offset m.peer_delay in
  if testA && testB && testC then Some m else None

let end_of_roundtrip ?(tags = Logs.Tag.empty) t t1 t4 pkt auth =
  (* If a key is configured for this source, require the response to carry a
     valid MAC with that key (chrony's [NAU_CheckResponseAuth]). Without a key,
     accept regardless of any MAC. *)
  let authentication =
    let fn k = match auth with `Valid kid -> kid = k.Auth.id | _ -> false in
    Option.map fn t.key |> Option.value ~default:true
  in
  if (not authentication) && valid_packet ~t1 pkt then
    Logs.warn (fun m ->
        let tags = Logs.Tag.add source t tags in
        m ~tags "Discarding an unauthenticated response from %a:%d" Ipaddr.pp
          t.dst t.port);
  match (valid_packet ~t1 pkt && authentication, synced_packet pkt) with
  | true, true ->
      Logs.debug (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "%a" Packet.pp_meta pkt);
      Logs.debug (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "%a" Packet.pp pkt);
      Stats.set_ref_id t.stats ~ref_id:pkt.Packet.ref_id;
      t.remote_poll <- Some pkt.Packet.poll;
      t.stratum <- Some (Int.max pkt.Packet.stratum 0);
      t.leap <- (pkt.Packet.flags lsr 6) land 0x3;
      t.number_of_roundtrips <- t.number_of_roundtrips + 1;
      set_reachable t true;
      let sample = to_sample t (t1, t4) pkt in
      let fn sample =
        let open Sample in
        Logs.debug (fun m ->
            let tags = Logs.Tag.add source t tags in
            m ~tags "src=%a:%d reach=%a ts=%.09f offset=%e delay=%e disp=%e"
              Ipaddr.pp t.dst t.port Reachability.pp t.reachability
              Ptime.(Span.to_float_s (to_span sample.time))
              (Float.neg sample.offset) sample.root_delay sample.root_dispersion);
        (* How far the new sample is from what the prior samples predicted; like
           chrony, computed BEFORE accumulating the new sample. Offsets are
           stored negated, so we predict and compare in that convention. *)
        let estimated_offset = Stats.get_predict_offset t.stats sample.time in
        let error_in_estimate =
          Float.abs (Float.neg sample.offset -. estimated_offset)
        in
        Stats.accumulate t.stats sample;
        Stats.regression t.stats;
        (* Adapt the polling interval like chrony (ntp_core.c): grow it toward
           maxpoll when samples are plentiful and predictions are good, back off
           when the prediction error exceeds the peer distance. *)
        let peer_distance =
          sample.peer_dispersion +. (0.5 *. sample.peer_delay)
        in
        adjust_poll t (get_poll_adjusted t error_in_estimate peer_distance);
        (* A new sample is available: count it for the reference-update gate and
           flag it so the selection applies it once to [sel_score]. *)
        t.updates <- t.updates + 1;
        t.score_pending <- true;
        (* Count usable samples taken while flagged a falseticker by the last
           selection; a truthful sample resets it (cf. chrony replacing a
           persistent SRC_FALSETICKER pool source). *)
        if t.is_falseticker then t.falseticker_run <- t.falseticker_run + 1
        else t.falseticker_run <- 0
      in
      Option.iter fn sample
  | valid, synced ->
      if valid then t.remote_poll <- Some pkt.Packet.poll;
      set_reachable t (valid && synced)

let record_t1 _trigger ?(tags = Logs.Tag.empty) t (tx, rx) =
  let result = Sched.Computation.peek tx in
  let result = Option.get result in
  match (t.state, result) with
  | Server_unreachable, _ -> ()
  | End_of_round_trip, Error Discard ->
      Logs.debug (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "Roundtrip discarded by the receiver ")
  | (Sleep _ | Tx_sent _ | End_of_round_trip), _ ->
      invalid_transition ~state:"record_t1" t
  | New_round_trip _, Ok t1 -> t.state <- Tx_sent { t1 }
  | Rx_received { t4; pkt; auth }, Ok t1 ->
      end_of_roundtrip t t1 t4 pkt auth;
      t.state <- End_of_round_trip
  | _, Error Route_unreachable ->
      Logs.warn (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "Server unreachable");
      set_reachable t false;
      t.state <- Server_unreachable;
      (* NOTE(dinosaure): here, [Computation.cancel] will execute [record_t4]
         iff was not signaled by the user. By this way, we clean-up everything.
       *)
      ignore (Sched.Computation.cancel rx Discard)
  | _, Error Discard -> ()
  | _, Error exn ->
      Logs.err (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "Unexpected exception: %s" (Printexc.to_string exn))

let record_t4 _trigger ?(tags = Logs.Tag.empty) t (rx, tx) =
  let result = Sched.Computation.peek rx in
  let result = Option.get result in
  match (t.state, result) with
  | Server_unreachable, _ -> ()
  | New_round_trip _, Ok (t4, pkt, auth) ->
      t.remote_poll <- Some pkt.Packet.poll;
      t.state <- Rx_received { t4; pkt; auth }
  | Tx_sent { t1 }, Ok (t4, pkt, auth) ->
      end_of_roundtrip t t1 t4 pkt auth;
      t.state <- End_of_round_trip
  | (End_of_round_trip | Sleep _ | Rx_received _), _ ->
      invalid_transition ~state:"record_t4" t
  | _, Error Timeout ->
      t.state <- End_of_round_trip;
      set_reachable t false;
      Log.warn (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "Server (%a:%d) timeout (after %d roundtrip(s))" Ipaddr.pp
            t.dst t.port t.number_of_roundtrips);
      (* NOTE(dinosaure): here, [Sched.Computation.cancel] will execute
         [record_t1] iff was not signaled by the user. By this way, we clean-up
         everything.
       *)
      ignore (Sched.Computation.cancel tx Discard)
  | _, Error Discard -> ()
  | _, Error exn ->
      Logs.err (fun m ->
          let tags = Logs.Tag.add source t tags in
          m ~tags "Unexpected exception: %s" (Printexc.to_string exn))

let new_round_trip ?(tags = Logs.Tag.empty) trigger t () =
  match t.state with
  | Server_unreachable -> ()
  | Sleep { sleeper; ns= _ } ->
      if trigger == sleeper then begin
        let poll =
          match t.remote_poll with
          | Some remote_poll -> Int.max remote_poll 6
          | None -> 6
        in
        let pkt =
          {
            Packet.flags= 0x23 (* NTPv4, Client mode *)
          ; stratum= 0
          ; poll
          ; precision= 32 (* Don't reveal local time or state of the clock *)
          ; root_delay= 0.
          ; root_dispersion= 0.
          ; ref_id= 0
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
        assert (Sched.Trigger.on_signal ttx t (send, comp) (record_t1 ~tags));
        assert (Sched.Trigger.on_signal trx t (comp, send) (record_t4 ~tags));
        assert (Sched.Computation.attach send ttx);
        assert (Sched.Computation.attach comp trx);
        let port = String.get_uint16_ne (Mirage_crypto_rng.generate 2) 0 in
        let recv = { src= t.dst; port; comp } in
        t.state <- New_round_trip { port; pkt; send; recv }
      end
      else
        Logs.warn (fun m ->
            let tags = Logs.Tag.add source t tags in
            m ~tags "Unexpected trigger")
  | New_round_trip _ | Tx_sent _ | Rx_received _ | End_of_round_trip ->
      invalid_transition ~state:"new_round_trip" t

let float_sec_to_nsec v =
  let frac, sec = Float.modf v in
  let frac_ns = frac *. 1e9 in
  let ns = sec *. 1e9 in
  Float.to_int (ns +. frac_ns)

let handle ?(tags = Logs.Tag.empty) t =
  if is_dead t && t.falseticker_run >= _MAX_FALSETICKER_RUN then `Falseticker
  else if is_dead t then `Server_unreachable
  else
    match t.state with
    | Server_unreachable -> `Server_unreachable
    | New_round_trip { port; pkt; send; recv } -> `Send (port, pkt, send, recv)
    | Sleep _ | Tx_sent _ | Rx_received _ -> `Await
    | End_of_round_trip when Stats.samples t.stats < 6 ->
        (* NOTE(dinosaure): we would like to speed up the initial synchronisation.
         So we would like to start with a burst of 4-8 requests in order to make
         the first update of the clock sooner.

         TODO(dinosaure): we probably can set an option and formalize better the
         burst mode (known as iburst for chrony). *)
        let sleeper = Sched.Trigger.create () in
        let nsec (* 2sec *) = 2_000_000_000 in
        Logs.debug (fun m ->
            let tags = Logs.Tag.add source t tags in
            m ~tags "Sleep %a" Duration.pp (Int64.of_int nsec));
        t.state <- Sleep { sleeper; ns= nsec };
        assert (Sched.Trigger.on_signal sleeper t () new_round_trip);
        `Sleep (sleeper, nsec)
    | End_of_round_trip ->
        let sleeper = Sched.Trigger.create () in
        let sec = get_transmit_delay t in
        let nsec = float_sec_to_nsec sec in
        Logs.debug (fun m ->
            let tags = Logs.Tag.add source t tags in
            m ~tags "Sleep %a" Duration.pp (Int64.of_int nsec));
        t.state <- Sleep { sleeper; ns= nsec };
        assert (Sched.Trigger.on_signal sleeper t () new_round_trip);
        `Sleep (sleeper, nsec)

let on_slew t ~raw:_ ~cooked ~dfreq ~doffset =
  Stats.slew_samples t.stats cooked dfreq doffset

let make ?(port = 123) ?key dst =
  let pkt =
    {
      Packet.flags= 0x23 (* NTPv4, Client mode *)
    ; stratum= 0
    ; poll= 0
    ; precision= 32 (* Don't reveal local time or state of the clock *)
    ; root_delay= 0.
    ; root_dispersion= 0.
    ; ref_id= 0
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
  let stats = Stats.make (dst, port) in
  let t =
    {
      dst
    ; port
    ; state
    ; remote_poll= None
    ; number_of_roundtrips= 0
    ; stats
    ; poll_score= 0.
    ; local_poll= 6 (* SRC_DEFAULT_MINPOLL = 6 *)
    ; reachability= Reachability.make ()
    ; unreachable_run= 0
    ; is_falseticker= false
    ; falseticker_run= 0
    ; key
    ; stratum= None
    ; leap= 0
    ; sel_score= 1.0
    ; selected= false
    ; updates= 0
    ; score_pending= false
    ; distant= 0
    }
  in
  assert (Sched.Trigger.on_signal ttx t (send, comp) (record_t1 ?tags:None));
  assert (Sched.Trigger.on_signal trx t (comp, send) (record_t4 ?tags:None));
  assert (Sched.Computation.attach send ttx);
  assert (Sched.Computation.attach comp trx);
  Clock.register_on_slew (on_slew t);
  t

let wake_up sleeper = Sched.Trigger.signal sleeper
let tx_sent tx ts = ignore (Sched.Computation.return tx ts)

let rx_received ~src ~src_port ~ts ~auth pkt (rx : rx) =
  if Ipaddr.compare rx.src src == 0 && src_port == rx.port then
    ignore (Sched.Computation.return rx.comp (ts, pkt, auth))

let rx_active rx = Sched.Computation.is_running rx.comp
let rx_port ({ port; _ } : rx) = port
let dst_unreachable tx = ignore (Sched.Computation.cancel tx Route_unreachable)
let rx_timeout rx = ignore (Sched.Computation.cancel rx.comp Timeout)
