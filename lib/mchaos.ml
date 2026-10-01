let src = Logs.Src.create "chaos.mkernel"

module Log = (val Logs.src_log src : Logs.LOG)

module Wk = struct
  (* Level-triggered wake-up: an interruption arriving while no one is idle is
     latched into [pending] so the next [idle] returns immediately instead of
     blocking on a signal that already happened (avoids the lost-wakeup race).

     ---- weird case ----
     t0 | interrupt => t.pending <- true
     t1 | idle => non-blocking (considering our Wk.t as already "awake")

     ---- "normal" case ----
     t0 | idle => await
     t1 | interrupt => wake-up *)
  type t = {
      mutable pending: bool
    ; mutable waiter: unit Miou.Computation.t option
  }

  let create () = { pending= false; waiter= None }

  let interrupt t =
    match t.waiter with
    | Some c ->
        t.waiter <- None;
        ignore (Miou.Computation.try_return c ())
    | None -> t.pending <- true

  let idle t =
    if t.pending then t.pending <- false
    else begin
      let c = Miou.Computation.create () in
      t.waiter <- Some c;
      Miou.Computation.await_exn c
    end
end

type waker = Wk.t

let waker = Wk.create
let idle = Wk.idle
let interrupt = Wk.interrupt

(*
let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      begin match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error exn ->
          Log.err (fun m ->
              m "Unexpected exception from a task: %s" (Printexc.to_string exn));
          clean_up orphans
      end

let rec terminate orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> Miou.yield (); terminate orphans
  | Some (Some prm) ->
      begin match Miou.await prm with
      | Ok () -> terminate orphans
      | Error exn ->
          Log.err (fun m ->
              m "A promise terminated with an exception: %s"
                (Printexc.to_string exn));
          terminate orphans
      end
*)

let when_ntp_is_received _trigger ts wk =
  ts := Chaos.Clock.read_cooked_time ();
  Wk.interrupt wk

type reception =
  [ `Packet of Ptime.t * Chaos.Packet.t * Chaos.Auth.result * Ipaddr.t * int
  | `Unknown of int ]

type listeners = {
    orphans: reception Miou.orphans
  ; actives: (int, reception Miou.t) Hashtbl.t
}

let listeners () = { orphans= Miou.orphans (); actives= Hashtbl.create 0x10 }

let new_listener keys udp wk t port =
  match Hashtbl.find_opt t.actives port with
  | Some _ -> ()
  | None ->
      let trigger = Miou.Trigger.create () in
      let ts = ref Ptime.min in
      assert (Miou.Trigger.on_signal trigger ts wk when_ntp_is_received);
      let prm =
        Miou.async ~orphans:t.orphans @@ fun () ->
        let buf = Bytes.create 0x7ff in
        let len, (peer, _peer_port) =
          Mnet.UDP.recvfrom udp ~port ~trigger buf
        in
        let str = Bytes.sub_string buf 0 len in
        match Chaos.Packet.decode str with
        | Ok pkt ->
            let auth = Chaos.Auth.check keys str in
            `Packet (!ts, pkt, auth, peer, port)
        | Error _ -> `Unknown port
      in
      Hashtbl.add t.actives port prm

let kill active_ports port prm =
  if List.exists (Int.equal port) active_ports = false then
    let () = Miou.cancel prm in
    None
  else Some prm

let listen keys udp wk t rxs =
  let rxs = List.filter Chaos.Source.rx_active rxs in
  let active_ports = List.map Chaos.Source.rx_port rxs in
  let active_ports = List.sort_uniq Int.compare active_ports in
  Hashtbl.filter_map_inplace (kill active_ports) t.actives;
  let new_ports =
    List.filter (Fun.negate (Hashtbl.mem t.actives)) active_ports
  in
  List.iter (new_listener keys udp wk t) new_ports;
  match Miou.care t.orphans with
  | None | Some None -> rxs
  | Some (Some prm) ->
      let () =
        match Miou.await prm with
        | Ok (`Packet (ts, pkt, auth, src, src_port)) ->
            Hashtbl.remove t.actives src_port;
            List.iter
              (Chaos.Source.rx_received ~src ~src_port ~ts ~auth pkt)
              rxs
        | Ok (`Unknown port) -> Hashtbl.remove t.actives port
        | Error Miou.Cancelled -> ()
        | Error exn ->
            Log.err (fun m ->
                m "A listener terminated with an exception: %s"
                  (Printexc.to_string exn))
      in
      rxs

let kill t = Hashtbl.iter (fun _ prm -> Miou.cancel prm) t.actives

let rec step udp wk sleepers rxs source =
  match Chaos.Source.handle source with
  | `Send (src_port, pkt, tx, rx) ->
      let dst, port = Chaos.Source.server source in
      let _ =
        Miou.async ~orphans:sleepers @@ fun () ->
        Mkernel.sleep 3_000_000_000;
        Chaos.Source.rx_timeout rx;
        Wk.interrupt wk
      in
      let ts = ref Ptime.min in
      let ok _ = Chaos.Source.tx_sent tx !ts
      and error _ =
        Log.warn (fun m -> m "%a:%d unreachable" Ipaddr.pp dst port);
        Chaos.Source.dst_unreachable tx
      in
      let now () = Chaos.Clock.read_cooked_time () in
      let key = Chaos.Source.key source in
      let len = Option.map (fun _ -> 48 + Chaos.Auth.mac_length) key in
      let len = Option.value ~default:48 len in
      (* NOTE(dinosaure): [fn] is executed **after** the discovery
         of routes. When the new NTPv4 packet is sent, we have the most accurate
         time of transmission from the perspective of the unikernel. *)
      let fn bstr =
        ts := Chaos.Packet.encode_into ~now pkt bstr;
        Option.iter (fun k -> Chaos.Auth.append_into k bstr) key
      in
      Mnet.UDP.sendfn udp ~src_port ~dst ~port ~len fn |> Result.fold ~ok ~error;
      Wk.interrupt wk;
      step udp wk sleepers (rx :: rxs) source
  | `Await -> (`Continue, rxs)
  | `Falseticker | `Server_unreachable -> (`Stop, rxs)
  | `Sleep (sleeper, ns) ->
      let _ =
        Miou.async ~orphans:sleepers @@ fun () ->
        Mkernel.sleep ns;
        Chaos.Source.wake_up sleeper;
        Wk.interrupt wk
      in
      step udp wk sleepers rxs source
