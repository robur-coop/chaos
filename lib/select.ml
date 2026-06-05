let src = Logs.Src.create "chaos.combine"

module Log = (val Logs.src_log src : Logs.LOG)

let _MAX_CLOCK_ERROR = 1. *. 1e-6
let _MAX_DISTANCE = 3.0
let _MAX_JITTER = 1.0

let compare (_u_idx, u_offset, u_tag) (_v_idx, v_offset, v_tag) =
  if u_offset < v_offset then 0 - 1
  else if u_offset > v_offset then 0 + 1
  else
    match (u_tag, v_tag) with
    | `Low, `High -> 0 - 1
    | `High, `Low -> 0 + 1
    | _ -> 0

let search_interval sources =
  let fn (idx, acc) = function
    | `Bad_distance _ | `Jittery _ | `No_stats _ -> (succ idx, acc)
    | `Ok (_, info) ->
        let acc =
          (idx, info.Stats.lo_limit, `Low)
          :: (idx, info.Stats.hi_limit, `High)
          :: acc
        in
        (succ idx, acc)
  in
  (* XXX(dinosaure): we discard bad sources and collect lo/hi offsets. *)
  let _, sources = List.fold_left fn (0, []) sources in
  let ( let* ) = Option.bind in
  let* sources = match sources with [] -> None | _ :: _ -> Some sources in
  let endpoints = List.stable_sort compare sources in
  let depth = ref 0 in
  let best_depth = ref 0 in
  let best_lo = ref 0. in
  let best_hi = ref 0. in
  (* XXX(dinosaure): here, we search the smallest continuous interval.
     Result is <=> and intervals are <->

     Example 1:
     1: <------------>
     2:   <-->
     3:         <-->
          <========>

     Example 2:
     1: <------------->
     2:  <------->
     3:           <-->
     4:   <-->
          <==>

     We choose the interval depending on the "depth". In the Example 2,
     we have a depth interval of 3 which for the interval [4], so the interval
     [4] is better than the interval [3] for instance.

     The Example 1 is a worst case where the depth can not help us to
     determine the best interval between [2] and [3] so we take a merge of
     them and continue our choice based on the stratum and the stability.
   *)
  let fn = function
    | _idx, lo_limit, `Low ->
        incr depth;
        if !depth > !best_depth then begin
          best_depth := !depth;
          best_lo := lo_limit
        end
    | _idx, hi_limit, `High ->
        if !depth == !best_depth then best_hi := hi_limit;
        decr depth
  in
  List.iter fn endpoints;
  (* We should have an agreement between sources. *)
  Log.debug (fun m ->
      m "best-depth=%d selected-sources=%d" !best_depth
        (List.length endpoints / 2));
  if endpoints = [] || !best_depth <= List.length endpoints / 4 then None
  else Some (!best_lo, !best_hi)

let source_of = function
  | `Ok (source, _)
  | `Falseticker (source, _)
  | `Jittery source
  | `Bad_distance source
  | `No_stats source ->
      source

let in_interval ~lo ~hi sources =
  let fn = function
    | `Ok (source, info) ->
        let open Stats in
        if
          (info.lo_limit <= lo && info.hi_limit >= hi)
          || (info.lo_limit >= lo && info.hi_limit <= hi)
        then `Ok (source, info)
        else `Falseticker (source, info)
    | elt -> elt
  in
  List.map fn sources

let qualify ~now sources =
  let unreachable_sources = ref 0 in
  let nostats_sources = ref 0 in
  let max_reach_sample_ago = ref 0. in
  let fn source =
    (* TODO(dinosaure):
       - verify the LEAP indicator from the last packet of the given source *)
    if Source.is_reachable source == false then incr unreachable_sources;
    match Stats.get_selection_data (Source.stats source) now with
    | Some info ->
        let info =
          if info.Stats.first_sample_ago < 2. *. info.Stats.last_sample_ago then
            (* Include extra dispersion in the root distance of sources that
               don't have new samples (the last sample is older than span of all
               sample). *)
            let extra_disp =
              _MAX_CLOCK_ERROR
              *. ((2. *. info.last_sample_ago) -. info.first_sample_ago)
            in
            let root_distance = info.root_distance +. extra_disp in
            let lo_limit = info.lo_limit -. extra_disp in
            let hi_limit = info.hi_limit +. extra_disp in
            { info with root_distance; lo_limit; hi_limit }
          else info
        in
        (* Require the root distance to be below the allowed maximum and the
           endpoints to be in the right order (i.e. a non-negative distance).
           And the same applies for the estimated standard deviation. *)
        if
          not
            (info.root_distance <= _MAX_DISTANCE
            && info.lo_limit <= info.hi_limit)
        then `Bad_distance source
        else if info.std_dev > _MAX_JITTER then `Jittery source
        else begin
          if
            Source.is_reachable source
            && !max_reach_sample_ago < info.first_sample_ago
          then max_reach_sample_ago := info.first_sample_ago;
          `Ok (source, info)
        end
    | None -> incr nostats_sources; `No_stats source
  in
  let sources = List.map fn sources in
  Log.debug (fun m ->
      m "nostats=%d max-reach-ago=%f" !nostats_sources !max_reach_sample_ago);
  sources

let find_minimum_stratum = function
  | [] -> None
  | sources ->
      let fn acc elt =
        match (acc, elt) with
        | Some default, `Ok (source, _) ->
            Some (Int.min default (Source.stratum ~default source))
        | None, `Ok (source, _) ->
            let stratum = Source.stratum ~default:Int.max_int source in
            if stratum == Int.max_int then None else Some stratum
        | acc, _ -> acc
      in
      List.fold_left fn None sources

let square x = x *. x
let _COMBINE_LIMIT = 3.0
let _RESELECT_DISTANCE = 1e-4
let _SCORE_LIMIT = 10.0
let _STRATUM_WEIGHT = 1e-3
let _DISTANT_PENALTY = 32
let _SOURCE_REACH_BITS = 8

(* Leap second vote among the selectable sources, like chrony's
   [get_leap_status]: a leap is accepted only if more than half of the voting
   sources agree. Returns the NTP leap encoding (0 normal, 1 insert, 2 delete). *)
let leap_of scored =
  let votes = ref 0 and ins = ref 0 and del = ref 0 in
  List.iter
    (function
      | _, `Ok (source, _) ->
          incr votes;
          (match Source.leap source with
          | 1 -> incr ins
          | 2 -> incr del
          | _ -> ())
      | _ -> ())
    scored;
  if !ins > !votes / 2 then 1 else if !del > !votes / 2 then 2 else 0

(* Selection distance of a source: its root distance penalised by the stratum
   difference and, for NTP sources, a small reselect distance (cf. chrony). *)
let distance_of ~min_stratum source info =
  let diff = Source.stratum ~default:min_stratum source - min_stratum in
  info.Stats.root_distance
  +. (Float.of_int diff *. _STRATUM_WEIGHT)
  +. _RESELECT_DISTANCE

let combine (sel_idx, sel_source, sel_info, sel_data) sources =
  let open Stats in
  let sum_offset_weight = ref 0.
  and sum_offset = ref 0.
  and sum2_offset_sd = ref 0.
  and sum_frequency_weight = ref 0.
  and sum_frequency = ref 0.
  and inv_sum2_frequency_sd = ref 0.
  and inv_sum2_skew = ref 0.
  and combined_sources = ref 0 in
  (* NOTE(dinosaure): like chrony's [combine_sources], the selected source is
     part of the weighted average (with [elapsed = 0]). A non-selected source
     whose root distance is much larger than the selected one's, or whose
     estimated frequency is too far, is flagged "distant" and excluded. The
     [distant] counter gives this decision hysteresis: once a mature source is
     flagged it is penalised for [_DISTANT_PENALTY] reference updates before it
     can rejoin the combination (a single update during warm-up). *)
  let sel_src_distance = sel_info.root_distance +. _RESELECT_DISTANCE in
  let fn idx (_score, elt) =
    match elt with
    | `Ok (source, info) ->
        let data = Stats.get_tracking_data (Source.stats source) in
        let too_far =
          idx != sel_idx
          && (info.root_distance > _COMBINE_LIMIT *. sel_src_distance
             || Float.abs (sel_data.frequency -. data.frequency)
                > _COMBINE_LIMIT
                  *. (sel_data.skew +. data.skew +. _MAX_CLOCK_ERROR))
        in
        if too_far then
          Source.set_distant source
            (if Source.reachability_size source >= _SOURCE_REACH_BITS then
               _DISTANT_PENALTY
             else 1)
        else if Source.distant source > 0 then
          Source.set_distant source (Source.distant source - 1);
        if Source.distant source > 0 then ()
        else begin
          incr combined_sources;
          let elapsed =
            Ptime.(Span.to_float_s (diff sel_data.ref_time data.ref_time))
          in
          let offset = data.offset +. (elapsed *. data.frequency) in
          let offset_sd = data.offset_sd +. (elapsed *. data.frequency_sd) in
          let offset_weight = 1.0 /. info.root_distance in
          let frequency_weight = 1.0 /. square data.frequency_sd in
          sum_offset_weight := !sum_offset_weight +. offset_weight;
          sum_offset := !sum_offset +. (offset_weight *. offset);
          sum2_offset_sd :=
            !sum2_offset_sd
            +. offset_weight
               *. (square offset_sd +. square (offset -. sel_data.offset));
          sum_frequency_weight := !sum_frequency_weight +. frequency_weight;
          sum_frequency := !sum_frequency +. (frequency_weight *. data.frequency);
          inv_sum2_frequency_sd :=
            !inv_sum2_frequency_sd +. (1.0 /. square data.frequency_sd);
          inv_sum2_skew := !inv_sum2_skew +. (1.0 /. square data.skew);
          let addr, port = Source.server source in
          Log.debug (fun m ->
              m
                "combining %a:%d oweight=%e offset=%e osd=%e fweight=%e \
                 freq=%e fsd=%e skew=%e"
                Ipaddr.pp addr port offset_weight data.offset data.offset_sd
                frequency_weight data.frequency data.frequency_sd data.skew)
        end
    | _ -> ()
  in
  List.iteri fn sources;
  let ref_time = sel_data.ref_time in
  let offset = !sum_offset /. !sum_offset_weight in
  let offset_sd = Float.sqrt (!sum2_offset_sd /. !sum_offset_weight) in
  let frequency = !sum_frequency /. !sum_frequency_weight in
  let frequency_sd = 1.0 /. Float.sqrt !inv_sum2_frequency_sd in
  let skew = 1.0 /. Float.sqrt !inv_sum2_skew in
  let root_delay = sel_data.root_delay in
  let root_dispersion = sel_data.root_dispersion in
  Log.debug (fun m ->
      m "combined result offset=%e osd=%e freq=%e fsd=%e skew=%e" offset
        offset_sd frequency frequency_sd skew);
  let data =
    {
      Stats.ref_time
    ; offset
    ; offset_sd
    ; frequency
    ; frequency_sd
    ; skew
    ; root_delay
    ; root_dispersion
    }
  in
  (sel_source, data, !combined_sources)

let select now sources0 =
  let ( let* ) = Option.bind in
  let qualified = qualify ~now sources0 in
  let* lo, hi = search_interval qualified in
  Logs.debug (fun m -> m "interval lo=%f hi=%f" lo hi);
  (* TODO(dinosaure): filter sources against orphan stratum *)
  let qualified = in_interval ~lo ~hi qualified in
  let* min_stratum = find_minimum_stratum qualified in
  (* The current reference source, if it is still selectable. *)
  let selected_ok =
    let fn = function
      | `Ok (source, info) when Source.selected source -> Some (source, info)
      | _ -> None in
    List.find_map fn qualified
  in
  let sel_distance =
    Option.map (fun (s, i) -> distance_of ~min_stratum s i) selected_ok
  in
  let sel_pending =
    Option.map (fun (s, _) -> Source.score_pending s) selected_ok
    |> Option.value ~default:false
  in
  (* Update the persistent scores (the hysteresis). A non-selectable source has
     its score reset to 1.0. For a selectable source, when a reference already
     exists we multiply its score by [sel_distance / distance] (clamped to
     >= 1.0), but only if it or the reference has a fresh sample (so a sample is
     scored exactly once); otherwise the score is simply the inverse distance. *)
  let scored =
    let fn elt = match elt with
      | `Ok (source, info) ->
        let distance = distance_of ~min_stratum source info in
        let score = match sel_distance with
          | Some sel_distance when Source.score_pending source || sel_pending ->
            let value = Source.sel_score source *. (sel_distance /. distance) in
            let value = Float.max 1.0 value in
            Source.set_sel_score source value;
            value
          | Some _ -> Source.sel_score source
          | None ->
            let value = 1.0 /. distance in
            Source.set_sel_score source value;
            value in
        (score, elt)
      | elt ->
        let source = source_of elt in
        Source.set_sel_score source 1.0;
        (* A non-selectable source starts penalised when it becomes Ok again. *)
        Source.set_distant source _DISTANT_PENALTY;
        (1.0, elt) in
    List.map fn qualified
  in
  (* The pending samples have now been accounted for in the scores. *)
  List.iter (fun s -> Source.set_score_pending s false) sources0;
  List.iter
    (fun (score, elt) ->
      let addr, port = Source.server (source_of elt) in
      Log.debug (fun m -> m "%a:%d score=%f" Ipaddr.pp addr port score))
    scored;
  (* Source with the maximum score amongst the selectable ones. *)
  let max_src =
    List.fold_left
      (fun acc (score, elt) ->
        match (elt, acc) with
        | `Ok (source, info), Some (_, _, best) when score > best ->
            Some (source, info, score)
        | `Ok (source, info), None -> Some (source, info, score)
        | _ -> acc)
      None scored
  in
  let* max_source, max_info, max_score = max_src in
  let has_selected = Option.is_some selected_ok in
  (* Switch the reference only if there is none yet (or it is no longer
     selectable), or another source has accumulated a score above the limit. *)
  let switch =
    (not has_selected)
    || ((not (Source.selected max_source)) && max_score > _SCORE_LIMIT)
  in
  let chosen =
    if switch then
      if Source.updates max_source = 0 then None
        (* Wait until the new reference can actually update the clock. *)
      else begin
        Log.debug (fun m ->
            let addr, port = Source.server max_source in
            m "selecting new reference %a:%d (score=%f)" Ipaddr.pp addr port
              max_score);
        List.iter
          (fun s ->
            Source.set_selected s false;
            Source.set_sel_score s 1.0;
            Source.set_distant s 0)
          sources0;
        Source.set_selected max_source true;
        Some (max_source, max_info)
      end
    else selected_ok
  in
  let* sel_source, sel_info = chosen in
  (* Don't update the reference when the selected source has no new sample. *)
  if Source.updates sel_source = 0 then None
  else begin
    List.iter (fun s -> Source.set_updates s 0) sources0;
    let sel_data = Stats.get_tracking_data (Source.stats sel_source) in
    let sel_idx =
      let rec go i = function
        | [] -> assert false
        | (_, elt) :: tl ->
            if source_of elt == sel_source then i else go (succ i) tl
      in
      go 0 scored
    in
    let source, data, combined_sources =
      combine (sel_idx, sel_source, sel_info, sel_data) scored
    in
    Some (source, data, combined_sources, leap_of scored)
  end
