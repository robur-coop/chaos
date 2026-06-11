module Clock = Chaos.Clock
module Stats = Chaos.Stats
module Sample = Chaos.Sample
module Source = Chaos.Source
module Packet = Chaos.Packet
module Select = Chaos.Select

let () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"foo=" (module Mirage_crypto_rng.Fortuna))

module Mon = struct
  let v = Atomic.make 0
  let now () = Atomic.fetch_and_add v 1000

  let advance secs =
    Atomic.fetch_and_add v (int_of_float (secs *. 1e9)) |> ignore
end

let () = Clock.init Mon.now
let span a b = Ptime.Span.to_float_s (Ptime.diff a b)

let set_freq target =
  let f = Clock.frequency () in
  let dfreq = (target -. f) /. (1e6 -. f) in
  Clock.accumulate_freq_and_offset ~dfreq ~doffset:0.0

(* NOTE(dinosaure): must run first: relies on the initial freq=0, offset=0. *)
let clock_identity =
  Test.test ~title:"clock/identity"
    ~description:"with freq=0 and offset=0, cooked time = raw time"
  @@ fun () ->
  let raw = Clock.read_raw_time () in
  let cooked = Clock.cook raw in
  Test.check_float ~msg:"cooked=raw" (span cooked raw) 0.0

let clock_slew_rate =
  Test.test ~title:"clock/slew-rate"
    ~description:
      "a positive freq makes cooked advance at 1 - 1e-6*freq per raw s"
  @@ fun () ->
  set_freq 10.0 (* +10 ppm *);
  let r0 = Clock.read_raw_time () in
  let c0 = Clock.cook r0 in
  Mon.advance 100.0;
  let r1 = Clock.read_raw_time () in
  let c1 = Clock.cook r1 in
  let raw_delay = span r1 r0 in
  let cooked_delay = span c1 c0 in
  Test.check_float ~eps:1e-6 ~msg:"slewed rate" cooked_delay
    (raw_delay *. (1.0 -. (1e-6 *. 10.0)))

let clock_offset_jump =
  Test.test ~title:"clock/offset-jump"
    ~description:
      "applying doffset steps cooked by -doffset (and update_offset is \
       value-preserving, so the freq term cancels)"
  @@ fun () ->
  let r = Clock.read_raw_time () in
  let cooked_before = Clock.cook r in
  Clock.accumulate_freq_and_offset ~dfreq:0.0 ~doffset:0.5;
  let cooked_after = Clock.cook r in
  let delay = span cooked_before cooked_after in
  Test.check_float ~eps:1e-6 ~msg:"jump = doffset" delay 0.5

let clock_freq_clamp =
  Test.test ~title:"clock/freq-clamp"
    ~description:"frequency is clamped to +/- 5e5 ppm (keeps the slew monotone)"
  @@ fun () ->
  set_freq 1e7 (* too much! *);
  Test.check_float ~msg:"clamped to 5e5" (Clock.frequency ()) 5e5

let localhost = (Ipaddr.V4 Ipaddr.V4.localhost, 123)
let ptime_of_s s = Option.get (Ptime.of_float_s s)
let base = 1_000_000.0
let poll = 64.0

let feed ?(noise = fun _ -> 0.0) delays =
  let st = Stats.make ~min_samples:1 ~min_delay:0.0 ~asymmetry:0.0 localhost in
  let fn idx delay =
    let sample =
      Sample.make
        ~offset:(-0.001 +. noise idx)
        ~delay
        (base +. (float_of_int idx *. poll))
    in
    Stats.accumulate st sample; Stats.regression st
  in
  Array.iteri fn delays; st

let alternating i = if i mod 2 = 0 then 1e-4 else -1e-4

let stats_regression_constant =
  Test.test ~title:"stats/regression-constant"
    ~description:
      "a constant offset with no drift yields freq~0 and the offset back \
       (stored negated)"
  @@ fun () ->
  let st = feed (Array.make 10 0.01) in
  let d = Stats.get_tracking_data st in
  (* sample.offset = -0.001 -> stored +0.001 -> estimated_offset ~ +0.001 *)
  Test.check_float ~eps:1e-4 ~msg:"offset" d.Stats.offset 0.001;
  Test.check_float ~eps:1e-6 ~msg:"frequency ~ 0" d.Stats.frequency 0.0;
  Test.check ~msg:"skew small" (d.Stats.skew < 1e-3)

let stats_min_delay =
  Test.test ~title:"stats/min-delay"
    ~description:"min_round_trip_delay tracks the minimum delay, not the newest"
  @@ fun () ->
  (* min delay (0.005) at index 7, newest (index 9) is larger; the alternating
     noise prevents pruning so the whole window is kept. *)
  let st =
    feed ~noise:alternating
      [| 0.02; 0.02; 0.02; 0.02; 0.02; 0.02; 0.02; 0.005; 0.02; 0.02 |]
  in
  match Stats.get_delay_test_data st (ptime_of_s (base +. (10.0 *. poll))) with
  | Some (_, _, min_delay, _, _) ->
      Test.check_float ~eps:1e-9 ~msg:"min delay" min_delay 0.005
  | None -> Test.check ~msg:"expected delay test data (n >= 6)" false

let stats_root_distance =
  Test.test ~title:"stats/root-distance"
    ~description:
      "selection root_distance = root_delay/2 + root_dispersion at the sample \
       (chrony's REF distance)"
  @@ fun () ->
  let r = 0.02 and disp = 1e-3 in
  let st = Stats.make ~min_samples:1 ~min_delay:0.0 ~asymmetry:0.0 localhost in
  for i = 0 to 9 do
    let s =
      Sample.make
        ~offset:(-0.001 +. alternating i)
        ~dispersion:disp ~delay:r
        (base +. (float_of_int i *. poll))
    in
    Stats.accumulate st s; Stats.regression st
  done;
  match Stats.get_selection_data st (ptime_of_s (base +. (9.0 *. poll))) with
  | Some info ->
      Test.check_float ~eps:1e-4 ~msg:"root_distance" info.Stats.root_distance
        ((0.5 *. r) +. disp)
  | None -> Test.check ~msg:"regression_ok / selection data available" false

(* NOTE(dinosaure): Simulate a roundtrip with a server. *)
let roundtrip ?(dst = Ipaddr.V4 Ipaddr.V4.localhost) ?(stratum = 2) ?(leap = 0)
    ~t0 ~t1 ~t2 ~t3 () =
  let src = Source.make dst in
  (match Source.handle src with
  | `Send (port, _req, send, recv) ->
      let t0p = ptime_of_s t0 in
      Source.tx_sent send t0p (* local transmit = t0 *);
      let flags =
        (leap lsl 6) lor (4 lsl 3) lor 4
        (* leap | NTPv4 | server *)
      in
      let pkt =
        {
          Packet.flags
        ; stratum
        ; poll= 6
        ; precision= -20
        ; root_delay= 0.0
        ; root_dispersion= 0.0
        ; ref_id= 0x7f000001
        ; ref_ts= Some (ptime_of_s (t0 -. 1.0))
        ; org_ts= Some t0p
        ; rx_ts= Some (ptime_of_s t1)
        ; tx_ts= Some (ptime_of_s t2)
        }
      in
      Source.rx_received ~src:dst ~src_port:port ~ts:(ptime_of_s t3) ~auth:`None
        pkt recv
  | _ -> assert false);
  src

(* NOTE(dinosaure): here, we check basically what a NTP server should compute. *)
let source_offset_delay =
  Test.test ~title:"source/offset-delay"
    ~description:
      "offset and round-trip delay from t0..t3 match chrony's formulas (theta \
       = ((t1-t0)+(t2-t3))/2, delay = (t3-t0)-(t2-t1))"
  @@ fun () ->
  let theta = 0.100 and d = 0.020 and proc = 0.001 in
  let t0 = base in
  let t1 = t0 +. d +. theta in
  let t2 = t1 +. proc in
  let t3 = t2 -. theta +. d in
  let chrony_offset = (t1 -. t0 +. (t2 -. t3)) /. 2.0 in
  let chrony_delay = t3 -. t0 -. (t2 -. t1) in
  let src = roundtrip ~t0 ~t1 ~t2 ~t3 () in
  let data : Stats.data = Stats.get_tracking_data (Source.stats src) in
  Test.check_float ~eps:1e-5 ~msg:"offset = chrony theta"
    (Float.neg data.Stats.offset)
    chrony_offset;
  Test.check_float ~eps:1e-5 ~msg:"root_delay = chrony delay"
    data.Stats.root_delay chrony_delay

(* NOTE(dinosaure): And what happens when the [client -> server] = [server -> client] (symmetry)? *)
let source_symmetry =
  Test.test ~title:"source/symmetric-delay"
    ~description:
      "with symmetric delay, the measured offset = theta for any delay \
       magnitude (chrony's key assumption)"
  @@ fun () ->
  let theta = 0.05 in
  List.iter
    (fun d ->
      let t0 = base in
      let t1 = t0 +. d +. theta in
      let t2 = t1 +. 0.001 in
      let t3 = t2 -. theta +. d in
      let src = roundtrip ~t0 ~t1 ~t2 ~t3 () in
      let data : Stats.data = Stats.get_tracking_data (Source.stats src) in
      Test.check_float ~eps:1e-5
        ~msg:(Printf.sprintf "d=%g -> offset=theta" d)
        (Float.neg data.Stats.offset)
        theta)
    [ 0.001; 0.01; 0.1; 0.5 ]

(* NOTE(dinosaure): and when the delay is assymetric? *)
let source_asymmetry =
  Test.test ~title:"source/asymmetric-delay"
    ~description:
      "with asymmetric delay the offset error is exactly (d_fwd - d_bwd)/2 \
       (NTP's known limitation)"
  @@ fun () ->
  let theta = 0.05 and d_fwd = 0.030 and d_bwd = 0.010 in
  let t0 = base in
  let t1 = t0 +. d_fwd +. theta in
  let t2 = t1 +. 0.001 in
  let t3 = t2 -. theta +. d_bwd in
  let src = roundtrip ~t0 ~t1 ~t2 ~t3 () in
  let data : Stats.data = Stats.get_tracking_data (Source.stats src) in
  Test.check_float ~eps:1e-5 ~msg:"theta + (d_fwd - d_bwd)/2"
    (Float.neg data.Stats.offset)
    (theta +. ((d_fwd -. d_bwd) /. 2.0))

let source_gating =
  Test.test ~title:"source/leap-stratum-gating"
    ~description:
      "unsynchronised (leap=3) or bad-stratum (0, >=16) responses are not \
       accepted as samples"
  @@ fun () ->
  let n_samples ~leap ~stratum =
    let t0 = base in
    let t1 = t0 +. 0.02 in
    let t2 = t1 +. 0.001 in
    let t3 = t2 +. 0.02 in
    let src = roundtrip ~leap ~stratum ~t0 ~t1 ~t2 ~t3 () in
    Stats.samples (Source.stats src)
  in
  Test.check ~msg:"leap=3 rejected" (n_samples ~leap:3 ~stratum:2 = 0);
  Test.check ~msg:"stratum=0 rejected" (n_samples ~leap:0 ~stratum:0 = 0);
  Test.check ~msg:"stratum>=16 rejected" (n_samples ~leap:0 ~stratum:16 = 0);
  Test.check ~msg:"synced server accepted" (n_samples ~leap:0 ~stratum:2 = 1)

(* NOTE(dinosaure): here, we create a source which can be selected by our algorithm:
   - good stratum
   - [is_reachable]
   - [regression_ok]
   - [Source.updates src > 0]
 *)
let ready_source ?(delay = 0.02) ~dst ~theta () =
  (* one-way, so peer_delay = delay -> root_distance ~ delay/2 *)
  let d = delay /. 2.0 in
  let t0 = base in
  let t1 = t0 +. d +. theta in
  let t2 = t1 +. 0.001 in
  let t3 = t2 -. theta +. d in
  let src = roundtrip ~dst ~t0 ~t1 ~t2 ~t3 () in
  let st = Source.stats src in
  for i = 1 to 7 do
    let s =
      Sample.make
        ~offset:(theta +. alternating i)
        ~delay
        (base +. (float_of_int i *. poll))
    in
    Stats.accumulate st s; Stats.regression st
  done;
  Source.set_updates src 1;
  src

let select_falseticker =
  Test.test ~title:"select/falseticker"
    ~description:
      "with 2 agreeing sources and 1 far-off (falseticker), selection ignores \
       the liar (Marzullo), as chrony does"
  @@ fun () ->
  let v4 s = Ipaddr.V4 (Ipaddr.V4.of_string_exn s) in
  let a = ready_source ~dst:(v4 "127.0.0.1") ~theta:0.000 () in
  let b = ready_source ~dst:(v4 "127.0.0.2") ~theta:0.001 () in
  let c = ready_source ~dst:(v4 "127.0.0.3") ~theta:0.500 () in
  (* NOTE(dinosaure): Our liar is [c]. *)
  let now = ptime_of_s (base +. (7.0 *. poll)) in
  match Select.select now [ a; b; c ] with
  | Some (selected, data, _combined, _leap) ->
      let sel_ip = fst (Source.server selected) in
      let c_ip = fst (Source.server c) in
      Test.check ~msg:"liar not selected" (Ipaddr.compare sel_ip c_ip <> 0);
      (* combined offset stays in the agreeing cluster, not dragged to 0.5 *)
      Test.check_float ~eps:0.05 ~msg:"combined offset near cluster"
        data.Stats.offset 0.0
  | None ->
      Test.check ~msg:"selection should succeed (2 agreeing sources)" false

let v4 s = Ipaddr.V4 (Ipaddr.V4.of_string_exn s)

let same_ip x y =
  Ipaddr.compare (fst (Source.server x)) (fst (Source.server y)) = 0

let select_no_quorum =
  Test.test ~title:"select/no-quorum"
    ~description:
      "two sources that disagree (no majority) -> selection returns None"
  @@ fun () ->
  let a = ready_source ~dst:(v4 "127.0.1.1") ~theta:0.000 () in
  let b = ready_source ~dst:(v4 "127.0.1.2") ~theta:0.500 () in
  let now = ptime_of_s (base +. (7.0 *. poll)) in
  match Select.select now [ a; b ] with
  | None -> Test.check ~msg:"no quorum -> None" true
  | Some _ -> Test.check ~msg:"expected None (no majority)" false

let select_best_distance =
  Test.test ~title:"select/best-distance"
    ~description:
      "among agreeing sources, the one with the smallest root distance becomes \
       the reference"
  @@ fun () ->
  let near = ready_source ~delay:0.004 ~dst:(v4 "127.0.2.1") ~theta:0.000 () in
  let far = ready_source ~delay:0.080 ~dst:(v4 "127.0.2.2") ~theta:0.000 () in
  let now = ptime_of_s (base +. (7.0 *. poll)) in
  (* [far] first, so it is not just a "first wins". *)
  match Select.select now [ far; near ] with
  | Some (selected, _, _, _) ->
      Test.check ~msg:"closest source selected" (same_ip selected near)
  | None -> Test.check ~msg:"selection should succeed" false

let select_bad_distance =
  Test.test ~title:"select/bad-distance"
    ~description:
      "a source beyond MAX_DISTANCE (root_distance > 3 s) is not selectable \
       nor combined"
  @@ fun () ->
  let good = ready_source ~dst:(v4 "127.0.3.1") ~theta:0.000 () in
  let bad = ready_source ~delay:8.0 ~dst:(v4 "127.0.3.2") ~theta:0.000 () in
  let now = ptime_of_s (base +. (7.0 *. poll)) in
  match Select.select now [ good; bad ] with
  | Some (selected, _, combined, _) ->
      Test.check ~msg:"good source selected" (same_ip selected good);
      Test.check ~msg:"the bad source is excluded from the combination"
        (combined = 1)
  | None -> Test.check ~msg:"selection should succeed (1 good source)" false

let select_hysteresis =
  Test.test ~title:"select/hysteresis"
    ~description:
      "the reference is sticky: a marginally-better challenger does not steal \
       it for several rounds (anti-flapping), but a better one eventually wins"
  @@ fun () ->
  let a = ready_source ~delay:0.020 ~dst:(v4 "127.0.4.1") ~theta:0.000 () in
  let b = ready_source ~delay:0.018 ~dst:(v4 "127.0.4.2") ~theta:0.000 () in
  let now = ptime_of_s (base +. (7.0 *. poll)) in
  (* a sample arrived for both sources this round *)
  let round srcs =
    List.iter
      (fun s ->
        Source.set_updates s 1;
        Source.set_score_pending s true)
      [ a; b ];
    Select.select now srcs
  in
  (* establish [a] as the reference first (alone) *)
  Source.set_updates a 1;
  ignore (Select.select now [ a ]);
  Test.check ~msg:"a is the reference" (Source.selected a);
  (* [b] is marginally better, but [a] must stay for the first few rounds *)
  let stayed = ref true in
  for _ = 1 to 3 do
    match round [ a; b ] with
    | Some (sel, _, _, _) -> if not (same_ip sel a) then stayed := false
    | None -> ()
  done;
  Test.check ~msg:"reference sticky for a few rounds (no flap)" !stayed;
  (* but eventually the consistently-better source wins *)
  let switched = ref false in
  for _ = 1 to 80 do
    match round [ a; b ] with
    | Some (sel, _, _, _) -> if same_ip sel b then switched := true
    | None -> ()
  done;
  Test.check ~msg:"the better source eventually wins" !switched

let () =
  Test.run
    [
      clock_identity; clock_slew_rate; clock_offset_jump; clock_freq_clamp
    ; stats_regression_constant; stats_min_delay; stats_root_distance
    ; source_offset_delay; source_symmetry; source_asymmetry; source_gating
    ; select_falseticker; select_no_quorum; select_best_distance
    ; select_bad_distance; select_hysteresis
    ]
