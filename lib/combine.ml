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
        (List.length endpoints / 4));
  if !best_depth <= List.length endpoints / 4 then None
  else Some (!best_lo, !best_hi)

let score ~min_stratum sources =
  let fn = function
    | `Ok (source, info) as elt ->
        let open Stats in
        let diff = State.stratum ~default:min_stratum source - min_stratum in
        let distance = info.root_distance +. (Float.of_int diff *. 1e-3) in
        let distance = distance +. 1e-4 in
        let score = 1.0 /. distance in
        (score, 0, elt)
    | (`Jittery _ | `Bad_distance _ | `No_stats _ | `Falseticker _) as elt ->
        let score = 1.0 in
        let distant = 32 in
        (score, distant, elt)
  in
  let sources = List.map fn sources in
  let fn = function
    | score, distant, elt ->
        let ipaddr, port =
          match elt with
          | `Ok (source, _)
          | `Falseticker (source, _)
          | `Jittery source
          | `Bad_distance source
          | `No_stats source ->
              State.server source
        in
        Log.debug (fun m ->
            m "%a:%d score=%f dist=%d" Ipaddr.V4.pp ipaddr port score distant)
  in
  List.iter fn sources; sources

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
    if State.is_reachable source == false then incr unreachable_sources;
    match Stats.get_selection_data (State.stats source) now with
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
            State.is_reachable source
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
            Some (Int.min default (State.stratum ~default source))
        | None, `Ok (source, _) ->
            let stratum = State.stratum ~default:Int.max_int source in
            if stratum == Int.max_int then None else Some stratum
        | acc, _ -> acc
      in
      List.fold_left fn None sources

let select_source now sources =
  let ( let* ) = Option.bind in
  let sources = qualify ~now sources in
  let* lo, hi = search_interval sources in
  Logs.debug (fun m -> m "Interval lo:%f hi:%f" lo hi);
  (* TODO(dinosaure): filter sources against orphan stratum *)
  let sources = in_interval ~lo ~hi sources in
  let* min_stratum = find_minimum_stratum sources in
  let sources = score ~min_stratum sources in
  let* best_source =
    let fn (score', source') = function
      | score, _distant, `Ok (source, _info) when score > score' ->
          (score, Some source)
      | _ -> (score', source')
    in
    List.fold_left fn (0.0, None) sources |> snd
  in
  let _data = Stats.get_tracking_data (State.stats best_source) in
  None
