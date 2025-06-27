let src = Logs.Src.create "chaos.instance"

module Log = (val Logs.src_log src : Logs.LOG)

[@@@warning "-26-32"]

type local = {
    ts: Ptime.t
  ; err: float
  ; rx_duration: float
  ; net_correction: float
}

let zero_local =
  { ts= Ptime.min; err= 0.0; rx_duration= 0.0; net_correction= 0.0 }

let _NTP_MAX_STRATUM = 16
let _NTP_INVALID_STRATUM = 0
let _NTP_MAX_DISPERSION = 16.0

(* Maximum assumed frequency error in network corrections *)
let _MAX_NET_CORRECTION_FREQ = 100e-6

(* Maximum ratio of local intervals in the timestamp selection of the
   interleaved mode to prefer a sample using previous timestamps. *)
let _MAX_INTERLEAVED_L2L_RATIO = 0.1

(* Maximum allowed time for server to process client packet *)
let _MAX_SERVER_INTERVAL = 4.0

(* Maximum offset between two sane times *)
let _MAX_OFFSET = 4294967296.0

type t = {
    interleaved: bool
  ; mutable local_poll: int (* Log2 of polling interval at our end *)
  ; mutable remote_poll: int
        (* Log2 of server/peer's polling interval (recovered from received
         packet) *)
  ; mutable remote_root_delay: float (* Root delay from last valid packet *)
  ; mutable remote_root_dispersion: float
        (* Root dispersion from last valid packet *)
  ; mutable presend_minpoll: int
        (* If the current polling interval is at least this, an extra client
         packet will be send some time before normal transmit. This ensures that
         both us and the server/peer have an ARP entry for each other ready,
         which means our measurement is not botched by an ARP round-trip on one
         side or the other. *)
  ; mutable presend_done: int (* The presend packet has been sent *)
  ; mutable minpoll: int (* Log2 of minimum defined polling interval *)
  ; mutable maxpoll: int (* Log2 of maximum defined polling interval *)
  ; mutable max_delay: float
        (* Maximum round-trip delay to the peer that we can tolerate and still use
         the sample for generating statistics from *)
  ; mutable max_delay_ratio: float
        (* Largest ratio of [delay] / [min_delay_in_register] that we can
         tolerate *)
  ; mutable max_delay_dev_ratio: float
        (* Maximum ratio of increase in [delay] / [stdder] *)
  ; mutable offset_correction: float
        (* Correction applied to measured offset (e.g. for asymmetry in network
         delay) *)
  ; mutable valid_timestamps: bool
        (* Flag indicating the timestamps below are from a valid packet and may be
         used for synchronisation *)

        (* Receive and transmit timestamps from the last valid response *)
  ; mutable remote_ntp_rx: Ptime.t
  ; mutable remote_ntp_tx: Ptime.t
        (* Local timestamp when last valid response was received from the source.
         We have to be prepared to tinker with this if the local clock has its
         frequency adjusted before we respond. The value we store here is what
         our own local time was when the same arrived. Before replying, we have
         to correct this to fit with the parameters for the current reference.
         It must be stored relative to local time to permit frequency and offset
         adjustments to be made when we trim the local clock. *)
  ; mutable local_ntp_rx: Ptime.t
  ; mutable local_rx: local
        (* Local timestamp when we last transmitted a packet to the source. We
         store two versions. The first is in NTP format, and is used to validate
         the next received packet from the source.

         Additionally, this is corrected to bring it into line with the current
         reference. The second is in [Ptime.t] format, and is kept relative to
         the local clock. We modify this in accordance with local clock
         frequency/offset changes, and use this for computing statistics about
         the source when a return packet arrives. *)
  ; mutable local_ntp_tx: Ptime.t
  ; mutable local_tx: local
        (* Previous values of some variables needed in interleaved mode *)
  ; mutable prev_local_tx: local
  ; mutable prev_local_poll: int
  ; source: Stats.t
}

let _MIN_POLL = -7
let _MAX_POLL = 24
let _DEFAULT_MIN_POLL = 6
let _DEFAULT_MAX_POLL = 10
let _MIN_NONLAN_POLL = 0
let _MAX_MAX_DELAY = 1e3
let _MAX_MAX_DELAY_RATIO = 1e6
let _MAX_MAX_DELAY_DEV_RATIO = 1e6
let clamp ~min:min_ ~max:max_ value = Float.max (Float.min value max_) min_

let make ?(interleaved = false) ?(minpoll = _DEFAULT_MIN_POLL)
    ?(maxpoll = _DEFAULT_MAX_POLL) ?(max_delay = 3.0) ?(max_delay_ratio = 0.0)
    ?(max_delay_dev_ratio = 10.0) ?(presend_minpoll = 100) ref_id =
  let minpoll =
    if minpoll < _MIN_POLL then _DEFAULT_MIN_POLL
    else if minpoll > _MAX_POLL then _MAX_POLL
    else minpoll
  in
  let maxpoll =
    if maxpoll < _MIN_POLL then _DEFAULT_MAX_POLL
    else if maxpoll > _MAX_POLL then _MAX_POLL
    else maxpoll
  in
  let max_delay = clamp ~min:0.0 ~max:_MAX_MAX_DELAY max_delay in
  let max_delay_ratio =
    clamp ~min:0.0 ~max:_MAX_MAX_DELAY_RATIO max_delay_ratio
  in
  let max_delay_dev_ratio =
    clamp ~min:0.0 ~max:_MAX_MAX_DELAY_DEV_RATIO max_delay_dev_ratio
  in
  let offset_correction = 0.0 in
  let local_poll = Int.max minpoll _MIN_NONLAN_POLL in
  let presend_done = 0 in
  let remote_poll = 0 in
  let remote_root_delay = 0.0 in
  let remote_root_dispersion = 0.0 in
  let remote_ntp_rx = Ptime.min in
  let remote_ntp_tx = Ptime.min in
  let local_rx = zero_local in
  let local_ntp_rx = Ptime.min in
  let local_tx = zero_local in
  let local_ntp_tx = Ptime.min in
  let prev_local_tx = zero_local in
  let prev_local_poll = 0 in
  let valid_timestamps = false in
  let source = Stats.make ref_id in
  {
    interleaved
  ; local_poll
  ; remote_poll
  ; remote_root_delay
  ; remote_root_dispersion
  ; presend_minpoll
  ; presend_done
  ; minpoll
  ; maxpoll
  ; max_delay
  ; max_delay_ratio
  ; max_delay_dev_ratio
  ; offset_correction
  ; valid_timestamps
  ; remote_ntp_rx
  ; remote_ntp_tx
  ; local_ntp_rx
  ; local_rx
  ; local_ntp_tx
  ; local_tx
  ; prev_local_tx
  ; prev_local_poll
  ; source
  }

let prefer_local_tss_on_interleaved_mode t =
  let open Ptime in
  let local_delay = Span.to_float_s (diff t.local_tx.ts t.local_rx.ts) in
  let prev_local_delay =
    Span.to_float_s (diff t.local_rx.ts t.prev_local_tx.ts)
  in
  _MAX_INTERLEAVED_L2L_RATIO *. local_delay > prev_local_delay

type metrics = {
    remote_rx: Ptime.t
  ; remote_tx: Ptime.t
  ; remote_req_rx: Ptime.t
  ; remote_avg: Ptime.t
  ; remote_interval: Ptime.span
  ; prev_remote_tx:
      Ptime.t option (* NOTE(dinosaure): it's Some only on interleaved mode *)
  ; local_rx: local
  ; local_tx: local
  ; local_avg: Ptime.t
  ; local_interval: Ptime.span
  ; root_delay: float
  ; root_dispersion: float
}

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

let log2_to_double l =
  let l = Int.max (Int.min l 31) (-31) in
  if l >= 0 then Float.of_int (1 lsl l) else 1. /. Float.of_int (1 lsl Int.abs l)

let _MIN_ENDOFTIME_DISTANCE = 365 * 24 * 3600

let is_time_offset_sane ts offset =
  if offset >= Float.neg _MAX_OFFSET && offset < _MAX_OFFSET then begin
    let t = Ptime.to_float_s ts +. offset in
    t >= 0.0 && t < Float.of_int (0x7fffffff - _MIN_ENDOFTIME_DISTANCE)
    (* NOTE(dinosaure): we should check larger value like [1 << 32] as the
        maximum. *)
  end
  else false

let check_delay_ratio local t stats sample_time delay =
  if t.max_delay_ratio < 1. then true
  else
    match Stats.get_delay_test_data stats sample_time with
    | None -> true
    | Some (last_sample_ago, _predicted_offset, min_delay, skew, _std_dev) ->
        let max_delay =
          (min_delay *. t.max_delay_ratio)
          +. (last_sample_ago *. (skew +. Local.max_clock_error local))
        in
        delay <= max_delay

let apply_net_correction (sample : Sample.t) rx tx precision =
  (* Require some correction from transparent clocks to be present in both
      directions (not just the local RX timestamp correction) *)
  if rx.net_correction >= rx.rx_duration && tx.net_correction <= 0. then begin
    (* With perfect corrections from PTP transparent clocks and short cables
         the peer delay would be close to zero, or even negative if the server
         or transparent clocks were running faster than client, which would
         invert the sample weighting. Adjust the correction to get a delay
         corresponding to a direct connection to the server. For simplicity,
         assume the TX and RX link speeds are equal. If not, the reported delay
         will be wrong, but it will not cause an error in the offset. *)
    let rx_correct = rx.net_correction -. rx.rx_duration in
    let tx_correct = tx.net_correction -. rx.rx_duration in
    let low_delay_correct =
      (rx_correct +. tx_correct) *. (1. -. _MAX_NET_CORRECTION_FREQ)
    in
    if low_delay_correct >= 0. || low_delay_correct < sample.Sample.peer_delay
    then
      let peer_delay = sample.peer_delay -. low_delay_correct in
      let peer_delay =
        if peer_delay < precision then precision else peer_delay
      in
      {
        sample with
        offset= sample.offset +. ((rx_correct -. tx_correct) /. 2.)
      ; peer_delay
      }
    else sample
  end
  else sample

let check_delay_dev_ratio local t stats sample_time offset delay =
  match Stats.get_delay_test_data stats sample_time with
  | None -> true
  | Some (last_sample_ago, predicted_offset, min_delay, skew, std_dev) ->
      (* Require that the ratio of the increase in delay from the minimum to the
         standard deviation is less than [max_delay_dev_ratio]. In the allowed
         increase in delay include also dispersion. *)
      let max_delta =
        (std_dev *. t.max_delay_dev_ratio)
        +. (last_sample_ago *. (skew +. Local.max_clock_error local))
      in
      let delta = (delay -. min_delay) /. 2. in
      if delta <= max_delta then true
      else
        let error_in_estimate = offset +. predicted_offset in
        Float.abs error_in_estimate -. delta > max_delta

(* This function allows you to collect 4 values whether you are in interleaved
   mode or not. These 4 values are those required to calculate the offset θ and
   the delay δ. *)
let choose_metrics ~interleaved_packet t rx pkt =
  (* Interleaved mode:

     Interleaved mode is an enhanced timestamp exchange method used in NTP to
     improve time synchronization accuracy. Instead of using timestamps taken
     immediately before and after processing a packet (which may be imprecise
     due to system delays), interleaved mode reuses timestamps from previous or
     future exchanges, taken under more stable conditions.

                    .- more accurate -.
                    |                 |
     Server    t2   ?            t6   t3
       --------+----+------------+----+---
              /      \          /      \
     Client  /        \        /        \
       -----+----------+------+----------+
            t1         t4     t5         t7
  *)
  if interleaved_packet then
    (* If the time allocated to our programme to send a new packet is not that
     long, we prefer to ‘forget’ the intermediate packets and use our first
     packet sent (with t1 and t2) as a reference rather than our second packet.

     NOTE(Chrony): Prefer previous local TX and previous remote RX timestamps if
     it will make the intervals significantly shorter in order to improve the
     accuracy of the measured delay *)
    if prefer_local_tss_on_interleaved_mode t (* 0.1 * (t5 - t7) > t5 - t4 *)
    then
      let remote_rx (* [t2] *) = t.remote_ntp_rx in
      let remote_tx (* [t3] *) = Option.get pkt.Packet.tx_ts in
      let remote_req_rx = remote_rx in
      let remote_avg, remote_interval =
        average_and_diff ~earlier:remote_rx ~later:remote_tx
      in
      let local_rx (* [t7] *) = t.local_rx in
      let local_tx (* [t1] *) = t.prev_local_tx in
      let local_avg, local_interval =
        average_and_diff ~earlier:local_tx.ts ~later:local_rx.ts
      in
      let prev_remote_tx = Some t.remote_ntp_tx in
      let root_delay = t.remote_root_delay in
      let root_dispersion = t.remote_root_dispersion in
      {
        remote_rx
      ; remote_tx
      ; remote_req_rx
      ; remote_avg
      ; remote_interval
      ; prev_remote_tx
      ; local_rx
      ; local_tx
      ; local_avg
      ; local_interval
      ; root_delay
      ; root_dispersion
      }
    else
      let remote_rx (* [t6] *) = Option.get pkt.Packet.rx_ts in
      let remote_tx (* [t3] *) = Option.get pkt.Packet.tx_ts in
      let remote_req_rx = t.remote_ntp_rx in
      let remote_avg, remote_interval =
        average_and_diff ~earlier:remote_rx ~later:remote_tx
      in
      let local_rx (* [t4] *) = t.local_rx in
      let local_tx (* [t5] *) = t.local_tx in
      let local_avg, local_interval =
        average_and_diff ~earlier:local_tx.ts ~later:local_rx.ts
      in
      let prev_remote_tx = Some t.remote_ntp_tx in
      let root_delay = Float.max pkt.Packet.root_delay t.remote_root_delay in
      let root_dispersion =
        Float.max pkt.Packet.root_dispersion t.remote_root_dispersion
      in
      {
        remote_rx
      ; remote_tx
      ; remote_req_rx
      ; prev_remote_tx
      ; remote_avg
      ; remote_interval
      ; local_rx
      ; local_tx
      ; local_avg
      ; local_interval
      ; root_delay
      ; root_dispersion
      }
  else
    let remote_rx (* [t2] *) = Option.get pkt.Packet.rx_ts in
    let remote_tx (* [t3] *) = Option.get pkt.Packet.tx_ts in
    let remote_req_rx = remote_rx in
    let remote_avg, remote_interval =
      average_and_diff ~earlier:remote_rx ~later:remote_tx
    in
    let local_rx (* [t4] *) = rx in
    let local_tx (* [t1] *) = t.local_tx in
    let local_avg, local_interval =
      average_and_diff ~earlier:local_tx.ts ~later:local_rx.ts
    in
    let root_delay = pkt.Packet.root_delay in
    let root_dispersion = pkt.Packet.root_dispersion in
    {
      remote_rx
    ; remote_tx
    ; remote_req_rx
    ; prev_remote_tx= None
    ; remote_avg
    ; remote_interval
    ; local_rx
    ; local_tx
    ; local_avg
    ; local_interval
    ; root_delay
    ; root_dispersion
    }

let process_response local t rx pkt =
  let pkt_leap = (pkt.Packet.flags lsr 6) land 0x3 in
  let _pkt_version = (pkt.Packet.flags lsr 3) land 0x7 in
  (* Test 1 checks for duplicate packet *)
  let test1 =
    let remote_ntp_rx = Packet.ptime_to_int64 t.remote_ntp_rx in
    let remote_ntp_tx = Packet.ptime_to_int64 t.remote_ntp_tx in
    Option.is_some pkt.Packet.rx_ts || Option.is_some pkt.Packet.tx_ts
  in
  (* Test 2 checks for bogus packet in the basic and interleaved modes. This
      ensure the source is responding to the latest packet we sent to it. *)
  let test2n =
    Ptime.compare t.local_ntp_tx (Option.get pkt.Packet.org_ts) == 0
  in
  let test2i =
    t.interleaved
    && Ptime.compare (Option.get pkt.Packet.org_ts) t.local_ntp_rx == 0
  in
  let test2 = test2n || test2i in
  let interleaved_packet = (not test2n) && test2i in
  (* Test 3 checks for invalid timestamps. This can happen when the association
      if not properly 'up'. *)
  let test3 =
    Option.is_some pkt.Packet.org_ts
    && Option.is_some pkt.Packet.rx_ts
    && Option.is_some pkt.Packet.tx_ts
  in
  (* Test 6 checks for unsynchronized server *)
  let test6 =
    pkt_leap != 3 (* Unsynchronized *)
    && pkt.Packet.stratum < _NTP_MAX_STRATUM
    && pkt.Packet.stratum != _NTP_INVALID_STRATUM
  in
  (* Test 7 checks for bad data. The root distance must be smaller than a
      defined maximum. *)
  let test7 =
    (pkt.Packet.root_delay /. 2.) +. pkt.Packet.root_dispersion
    < _NTP_MAX_DISPERSION
  in
  let valid_packet = test1 && test2 && test3 in
  let synced_packet = valid_packet && test6 && test7 in
  (* TODO(dinosaure): Kiss of Death. *)
  (* Some variables implie values. This graph show that some variables are
      [true] only if some others variables are [true]:

      good_packet => synced_packet => valid_packet => test1
                  => testA         => test6        => test2
                  => testB         => test7        => test3
                  => testC                         => test5
                  => testD
                  => (not interleaved_packet || t.valid_timestamps)
    *)
  if synced_packet && ((not interleaved_packet) || t.valid_timestamps) then begin
    let _mono_doffset = 0. and _net_correction = 0. in
    (* TODO(dinosaure): [mono_doffset] & [net_correction] are values which
         come from NTP extensions fields.
         See https://datatracker.ietf.org/doc/html/draft-mlichvar-ntp-correction-field-04
         for [net_correction] (the RFC seems expired). *)
    (* Select remote and local timestamps for the new sample *)
    let m = choose_metrics ~interleaved_packet t rx pkt in
    (* Calculate intervals between remote and local timestamps *)
    let response_time =
      Float.abs Ptime.(Span.to_float_s (diff m.remote_tx m.remote_req_rx))
    in
    let precision =
      Local.precision_as_quantum local +. log2_to_double pkt.precision
    in
    (* Calculate delay *)
    let peer_delay =
      Float.abs Ptime.Span.(to_float_s (sub m.local_interval m.remote_interval))
    in
    let peer_delay = if peer_delay < precision then precision else peer_delay in
    (* Calculate offset. Following the NTP definition, this is negative if
         we are fast of the remote source.

         Let's imagine that the packet transmission time is 0ps. We can
         consider that [t0 = t1]. Thus:

         x̄(r) = (t1 - t2) / 2 + t1
         x̄(l) = (t0 - t3) / 2 + t0

         θ = x̄(r) - x̄(l)
           = ((t1 - t2) / 2 + t1) - ((t0 - t3) / 2 + t0)
           = 1/2 (-3 t0 + 3 t1 - t2 + t3)
           = 1/2 (-t0 + t1 - t2 + t3) with t0 = t1
           = 1/2 ((t1 - t0) + (t3 - t2))
           = ((t1 - t0) + (t3 - t2)) / 2
         *)
    let offset = Ptime.(Span.to_float_s (diff m.remote_avg m.local_avg)) in
    let offset = offset +. t.offset_correction in
    (* We treat the time of the sample as being mdiway through the local
         measurement period. An analysis assuming constant relative frequency
         and zero network delay shows this is the only possible choice to
         estimate the frequency difference correctly for every sample pair. *)
    let time = m.local_avg in
    let src_freq_lo, src_freq_hi = Stats.get_frequency_range t.source in
    (* Calculate skew *)
    let skew = (src_freq_hi -. src_freq_lo) /. 2. in
    (* and then calculate peer dispersion and the rest of the sample *)
    let peer_dispersion =
      Float.max precision (Float.max m.local_tx.err m.local_rx.err)
      +. (skew *. Float.abs (Ptime.Span.to_float_s m.local_interval))
    in
    let root_delay = m.root_delay +. peer_delay in
    let root_dispersion = m.root_dispersion +. peer_dispersion in
    (* Apply corrections from PTP transparent clocks if available and sane *)
    let sample =
      apply_net_correction
        {
          Sample.time
        ; offset
        ; peer_delay
        ; peer_dispersion
        ; root_delay
        ; root_dispersion
        }
        m.local_rx m.local_tx precision
    in
    let prev_remote_poll_interval =
      log2_to_double (Int.min t.remote_poll t.prev_local_poll)
    in
    (* Test A combines multiple tests to avoid changing measurements log
         format and ntpdata report. It requires that:
         - it requires that the minimum estimate of the peer delay is not larger
           than the maximum,
         >   [sample.peer_delay -. sample.peer_dispersion <= t.max_delay]
         > & [precision <= t.max_delay]

         - it is not a response in the "warm up" exchange
         > [t.presend_done <= 0]

         - the configured offset correction is within the supported NTP interval
         > [is_time_offset_sane sample.time sample.offset]

         - the server processing time is sane
         > [repsonse_time <= _MAX_SERVER_INTERVAL]

         - in interleaved client/server mode thatthe previous response was not
           in base mode (which prevents using timestamps that minimise delay
           error) TODO(dinosaure): this test is in conflict with a special case
           of [choose_metrics] where we take [t.local_tx] as [t1] if the
           distance between [t4] and [t5] is short (see [choose_metrics])
         > [Ptime.compare m.local_tx.ts t.local_tx.tx != 0]
         
       *)
    let testA =
      match (m.prev_remote_tx, interleaved_packet) with
      | _, false ->
          sample.peer_delay -. sample.peer_dispersion <= t.max_delay
          && precision <= t.max_delay
          && t.presend_done <= 0
          && is_time_offset_sane sample.time sample.offset
          && not (response_time > _MAX_SERVER_INTERVAL)
      | Some _prev_remote_tx, true ->
          sample.peer_delay -. sample.peer_dispersion <= t.max_delay
          && precision <= t.max_delay
          && t.presend_done <= 0
          && is_time_offset_sane sample.time sample.offset
          && response_time <= _MAX_SERVER_INTERVAL
          && Ptime.compare m.local_tx.ts t.local_tx.ts != 0
      | None, true -> false
    in
    (* Test B requires in client mode that the ratio of the round trip delay
         to the minimum one currently in the stats data register is less than
         an administrator-defined value. *)
    let testB =
      check_delay_ratio local t t.source sample.time sample.peer_delay
    in
    (* Test C either requires that the delay is less than an estimate of an
         administrator-defined quantile (TODO), or (if the quantile is not
         specified) it requires that the ratio of the increase in delay from
         the minimum one in the stats data register to the standard deviation
         of the offsets in the register is less than an administrator-defined
         valuie or the difference between measured offset and predicted offset
         is larger than the increase in delay. *)
    let testC =
      check_delay_dev_ratio local t t.source sample.time sample.offset
        sample.peer_delay
    in
    assert false
    (* It cannot be a good packet. But it still by a valid or synced packet. *)
  end
  else assert false
