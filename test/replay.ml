(* This test replays a recorded chrony run and shows that chaos reproduces
   chrony's per-source regression. The log timestamps have only second
   resolution, which perturbs the gradient, so we make two claims: the very
   first regression matches chrony tightly (~1e-6: the math and sign convention
   are identical), and over the rest of the run chaos stays within a few ms of
   chrony (it does not drift away). The 6 ms here is the assertion bound, not
   the typical error. *)

module Stats = Chaos.Stats
module Sample = Chaos.Sample
module Clock = Chaos.Clock

module Mon = struct
  let v = Atomic.make 0
  let now () = Atomic.fetch_and_add v 1000
end

let () = Clock.init Mon.now (* NOTE(dinosaure): just calculate [precision]. *)

let sample (m : Dataset.measurement) : Sample.t =
  let time = Option.get (Ptime.of_float_s m.time) in
  {
    time
  ; offset= m.offset
  ; peer_delay= m.peer_delay
  ; peer_dispersion= m.peer_dispersion
  ; root_delay= m.root_delay
  ; root_dispersion= m.root_dispersion
  }

let _FIRST_EPS = 1e-4 (* identical math on the really first regression *)
let _BALLPARK = 6e-3 (* non-divergence bound (~6 ms) on real noisy data *)

let replay ip =
  let ms =
    Dataset.measurements "datasets/measurements.log"
    |> List.filter (fun (m : Dataset.measurement) -> m.ip = ip)
  in
  let ss =
    Dataset.statistics "datasets/statistics.log"
    |> List.filter (fun (s : Dataset.statistic) -> s.ip = ip)
  in
  let st =
    Stats.make ~min_samples:1 ~min_delay:0.0 ~asymmetry:1.0
      (Ipaddr.V4 (Ipaddr.V4.of_string_exn ip), 123)
  in
  let pending = ref ss in
  List.fold_left
    (fun acc (m : Dataset.measurement) ->
      Stats.accumulate st (sample m);
      Stats.regression st;
      match !pending with
      | s :: tl when s.Dataset.time <= m.time +. 0.5 ->
          pending := tl;
          let d : Stats.data = Stats.get_tracking_data st in
          (d.offset, s.est_offset) :: acc
      | _ -> acc)
    [] ms
  |> List.rev

let source ipaddr =
  Test.test
    ~title:(Fmt.str "replay/%s" ipaddr)
    ~description:
      "replay chrony measurements and compare the offset to statistics.log"
  @@ fun () ->
  let pairs = replay ipaddr in
  begin match pairs with
  | (c0, y0) :: _ ->
      let msg = Fmt.str "%s: first regression matches chrony" ipaddr in
      Test.check ~msg (Float.abs (c0 -. y0) < _FIRST_EPS)
  | [] ->
      let msg = Fmt.str "%s: has comparison points" ipaddr in
      Test.check ~msg false
  end;
  let fn a (c, y) = Float.max a (Float.abs (c -. y)) in
  let max_delta = List.fold_left fn 0. pairs in
  let msg =
    Fmt.str "%s: offset within %.0e of chrony (max=%.1e)" ipaddr _BALLPARK
      max_delta
  in
  Test.check ~msg (max_delta < _BALLPARK)

let () =
  Test.run
    [ source "82.64.42.185"; source "134.157.254.19"; source "129.104.30.42" ]
