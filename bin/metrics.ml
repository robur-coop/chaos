let _MAX_FIELD = 4e18

let to_int ~scale v =
  let v = Float.round (v *. scale) in
  if Float.is_nan v then 0
  else Float.to_int (Float.max (Float.neg _MAX_FIELD) (Float.min _MAX_FIELD v))

let ns = to_int ~scale:1e9
let ppb = to_int ~scale:1e9
let ms = to_int ~scale:1e3
let of_bool = Bool.to_int

let popcount v =
  let rec go acc v = if v = 0 then acc else go (acc + (v land 1)) (v lsr 1) in
  go 0 v

let source_name source =
  let ipaddr, port = Chaos.Source.server source in
  if port = 123 then Ipaddr.to_string ipaddr
  else Fmt.str "%a:%d" Ipaddr.pp ipaddr port

(* Like the first column of [chronyc sources]. *)
let state source =
  if not (Chaos.Source.is_reachable source) then 0 (* ? *)
  else if Chaos.Source.is_falseticker source then 1 (* x *)
  else if Chaos.Source.selected source then 3 (* * *)
  else 2 (* - or + *)

let tracking ~reference sources () =
  let open Chaos.Reference in
  let raw = Chaos.Clock.read_raw_time () in
  let now = Chaos.Clock.cook raw in
  let t = Chaos.Reference.tracking reference now in
  let sources = sources () in
  let selected = List.find_opt Chaos.Source.selected sources in
  let tags =
    [ ("reference", Option.fold ~none:"none" ~some:source_name selected) ]
  in
  let update_age =
    match t.last_ref_time with
    | Some ref_time -> Ptime.(Span.to_float_s (diff now ref_time))
    | None -> 0.0
  in
  let count fn = List.length (List.filter fn sources) in
  let fields =
    [
      ("synchronised", of_bool t.synchronised); ("stratum", t.stratum)
    ; ("leap", t.leap); ("last_offset_ns", ns t.last_offset)
    ; ("rms_offset_ns", ns t.rms_offset)
    ; ("frequency_ppb", to_int ~scale:1e3 (Chaos.Clock.frequency ()))
    ; ("residual_freq_ppb", ppb t.residual_freq); ("skew_ppb", ppb t.skew)
    ; ("root_delay_ns", ns t.root_delay)
    ; ("root_dispersion_ns", ns t.root_dispersion)
    ; ("offset_sd_ns", ns t.offset_sd); ("frequency_sd_ppb", ppb t.frequency_sd)
    ; ("remaining_correction_ns", ns (Chaos.Clock.pending_correction raw))
    ; ("update_age_ms", ms update_age); ("combined_sources", t.combined_sources)
    ; ("updates", t.updates); ("rejected_updates", t.rejected_updates)
    ; ("sources", List.length sources)
    ; ("reachable_sources", count Chaos.Source.is_reachable)
    ; ("falsetickers", count Chaos.Source.is_falseticker)
    ]
  in
  [ (tags, fields) ]

let source source =
  let open Chaos.Source in
  let stats = Chaos.Source.stats source in
  let counters = Chaos.Source.counters source in
  let reach = Chaos.Source.reachability_bits source in
  let poll = Chaos.Source.local_poll source in
  let last_sample =
    match Chaos.Source.last_sample source with
    | None -> []
    | Some s ->
        [
          ("last_offset_ns", ns (Float.neg s.Chaos.Sample.offset))
        ; ("last_delay_ns", ns s.peer_delay)
        ; ("last_dispersion_ns", ns s.peer_dispersion)
        ; ("root_delay_ns", ns s.root_delay)
        ; ("root_dispersion_ns", ns s.root_dispersion)
        ]
  in
  let estimations =
    if Chaos.Stats.samples stats <= 0 then []
    else
      let d = Chaos.Stats.get_tracking_data stats in
      [
        ("offset_ns", ns d.Chaos.Stats.offset); ("offset_sd_ns", ns d.offset_sd)
      ; ("frequency_ppb", ppb d.frequency); ("skew_ppb", ppb d.skew)
      ; ("std_dev_ns", ns (Chaos.Stats.std_dev stats))
      ]
  in
  let fields =
    [
      ("state", state source); ("selected", of_bool (selected source))
    ; ("falseticker", of_bool (is_falseticker source))
    ; ("stratum", stratum source); ("leap", leap source); ("reach", reach)
    ; ("reach_count", popcount reach); ("reach_size", reachability_size source)
    ; ("poll", poll); ("poll_interval_s", 1 lsl poll)
    ; ("remote_poll", Option.value ~default:0 (remote_poll source))
    ; ("samples", Chaos.Stats.samples stats)
    ; ("score_milli", to_int ~scale:1e3 (sel_score source))
    ; ("sent", counters.sent); ("received", counters.received)
    ; ("timeouts", counters.timeouts); ("unreachable", counters.unreachable)
    ; ("bad_packets", counters.bad_packets)
    ; ("accepted_samples", counters.accepted_samples)
    ; ("rejected_samples", counters.rejected_samples)
    ]
  in
  ([ ("source", source_name source) ], fields @ last_sample @ estimations)

let server srv () =
  let open Chaos.Server in
  let c = Chaos.Server.counters srv in
  [
    ( []
    , [
        ("requests", c.requests); ("responses", c.responses)
      ; ("authenticated", c.authenticated); ("kod", c.kod)
      ; ("bad_auth", c.bad_auth); ("ignored", c.ignored)
      ; ("clients", Chaos.Server.clients srv)
      ] )
  ]

let register ~reference ~server:srv sources =
  let _ = Tally.v "ntp_tracking" (tracking ~reference sources) in
  let _ = Tally.v "ntp_source" (fun () -> List.map source (sources ())) in
  let _ = Tally.v "ntp_server" (server srv) in
  ()
