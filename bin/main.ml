let () = Printexc.record_backtrace true

let rec clean_up_sleepers orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> ()
  | Some (Some prm) -> begin
      match Miou.await prm with
      | Ok () -> clean_up_sleepers orphans
      | Error exn ->
          Logs.err (fun m ->
              m "A sleeper terminated with an exception: %s"
                (Printexc.to_string exn));
          clean_up_sleepers orphans
    end

let rec terminate orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> Miou.yield (); terminate orphans
  | Some (Some prm) -> begin
      match Miou.await prm with
      | Ok () -> terminate orphans
      | Error exn ->
          Logs.err (fun m ->
              m "A promise terminated with an exception: %s"
                (Printexc.to_string exn));
          terminate orphans
    end

module Wk : sig
  type t

  val create : unit -> t
  val sleep : t -> int -> unit
  val interrupt : t -> unit
end = struct
  type t = Miou.Trigger.t ref

  let create () = ref (Miou.Trigger.create ())

  let cancel _trigger comp value =
    ignore (Miou.Computation.try_return comp value)

  let sleep wk ns =
    let tick = Miou.async @@ fun () -> Miou_solo5.sleep ns in
    let comp = Miou.Computation.create () in
    let proc = Miou.async @@ fun () -> Miou.Computation.await_exn comp in
    wk := Miou.Trigger.create ();
    assert (Miou.Trigger.on_signal !wk comp () cancel);
    ignore (Miou.await_first [ tick; proc ])

  let interrupt wk = Miou.Trigger.signal !wk
end

let when_ntpv4_is_received _trigger ts wk =
  ts := Tscclock.now ();
  Wk.interrupt wk

let new_listener orphans udpv4 actives wk port =
  let open Miou_solo5_net in
  match Hashtbl.find_opt actives port with
  | Some _ -> ()
  | None ->
      let trigger = Miou.Trigger.create () in
      let ts = ref 0 in
      assert (Miou.Trigger.on_signal trigger ts wk when_ntpv4_is_received);
      let prm =
        Miou.async ~orphans @@ fun () ->
        let buf = Bytes.create 0x7ff in
        let len, (peer, _peer_port) = UDPv4.recvfrom udpv4 ~port ~trigger buf in
        let str = Bytes.sub_string buf 0 len in
        match Chaos.Packet.decode str with
        | Ok pkt -> `Packet (Tscclock.of_int_ns !ts, pkt, peer, port)
        | Error _ -> `Unknown port
      in
      Hashtbl.add actives port prm

let cancel_listener active_ports port prm =
  if List.exists (Int.equal port) active_ports = false then
    let () = Miou.cancel prm in
    None
  else Some prm

let clean_up_listeners udpv4 orphans rxs actives wk =
  let rxs = List.filter Chaos.State.rx_active rxs in
  let active_ports = List.map Chaos.State.rx_port rxs in
  let active_ports = List.sort_uniq Int.compare active_ports in
  Hashtbl.filter_map_inplace (cancel_listener active_ports) actives;
  let new_ports = List.filter (Fun.negate (Hashtbl.mem actives)) active_ports in
  List.iter (new_listener orphans udpv4 actives wk) new_ports;
  match Miou.care orphans with
  | None | Some None -> rxs
  | Some (Some prm) ->
      let () =
        match Miou.await prm with
        | Ok (`Packet (ts, pkt, src, src_port)) ->
            Hashtbl.remove actives src_port;
            List.iter (Chaos.State.rx_received ~src ~src_port ~ts pkt) rxs
        | Ok (`Unknown port) -> Hashtbl.remove actives port
        | Error Miou.Cancelled -> ()
        | Error exn ->
            Logs.err (fun m ->
                m "A listener terminated with an exception: %s"
                  (Printexc.to_string exn))
      in
      rxs

let rec step udpv4 wk sleepers rxs server =
  let open Miou_solo5_net in
  match Chaos.State.handle server with
  | `Send (src_port, pkt, tx, rx) ->
      let dst, port = Chaos.State.server server in
      let _ =
        Miou.async ~orphans:sleepers @@ fun () ->
        Miou_solo5.sleep 3_000_000_000;
        Chaos.State.rx_timeout rx;
        Wk.interrupt wk
      in
      let ts = ref Ptime.min in
      let ok _ =
        Logs.debug (fun m ->
            m "-> %a:%d %a" Ipaddr.V4.pp dst port
              (Ptime.pp_human ~frac_s:9 ())
              !ts);
        Chaos.State.tx_sent tx !ts
      and error _ =
        Logs.debug (fun m -> m "%a:%d unreachable" Ipaddr.V4.pp dst port);
        Chaos.State.dst_unreachable tx
      in
      let now () = Tscclock.ptime () in
      (* NOTE(dinosaure): [fn] is executed **after** the discovery
         of routes. When the new NTPv4 packet is sent, we have the most accurate
         time of transmission from the perspective of the unikernel. *)
      let fn bstr = ts := Chaos.Packet.encode_into ~now pkt bstr in
      UDPv4.sendfn udpv4 ~src_port ~dst ~port ~len:48 fn
      |> Result.fold ~ok ~error;
      Wk.interrupt wk;
      step udpv4 wk sleepers (rx :: rxs) server
  | `Await -> `Continue (rxs, server)
  | `Error _ -> `Stop rxs
  | `Sleep (sleeper, ns) ->
      let _ =
        Miou.async ~orphans:sleepers @@ fun () ->
        Miou_solo5.sleep ns;
        Chaos.State.wake_up sleeper;
        Wk.interrupt wk
      in
      step udpv4 wk sleepers rxs server

let run udpv4 servers =
  let wk = Wk.create () in
  let actives = Hashtbl.create 0x10 in
  let local = Chaos.Local.make Tscclock.now in
  Logs.debug (fun m ->
      m "precision: %e" (Chaos.Local.precision_as_quantum local));
  let prm =
    Miou.async @@ fun () ->
    let sleepers = Miou.orphans () in
    let listeners = Miou.orphans () in
    let rec go rxs servers =
      clean_up_sleepers sleepers;
      let fn (servers, rxs) server =
        match step udpv4 wk sleepers [] server with
        | `Continue ([], server) -> (server :: servers, rxs)
        | `Continue (rxs', server) ->
            (server :: servers, List.rev_append rxs' rxs)
        | `Stop [] -> (servers, rxs)
        | `Stop rxs' -> (servers, List.rev_append rxs' rxs)
      in
      let servers, rxs = List.fold_left fn ([], rxs) servers in
      let rxs = clean_up_listeners udpv4 listeners rxs actives wk in
      match servers with
      | [] ->
          let prms = Hashtbl.to_seq_values actives in
          Seq.iter Miou.cancel prms; terminate sleepers
      | servers -> Wk.sleep wk 1_000_000_000; go rxs servers
    in
    go [] (List.map (Chaos.State.make ~local) servers)
  in
  Miou.await_exn prm

let run _ cidr gateway servers =
  Miou_solo5.(run [ Miou_solo5_net.stackv4 ~name:"service" ?gateway cidr ])
  @@ fun (daemon, _tcpv4, udpv4) () ->
  let _ = Tscclock.init () in
  let rng =
    Mirage_crypto_rng_miou_solo5.initialize (module Mirage_crypto_rng.Fortuna)
  in
  let finally () =
    Mirage_crypto_rng_miou_solo5.kill rng;
    Miou_solo5_net.kill daemon
  in
  Fun.protect ~finally @@ fun () ->
  Logs.debug (fun m -> m "tscclock_freq: %fGHz" (Tscclock.get_freq ()));
  run udpv4 servers

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Miou_solo5.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Miou_solo5.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity)

let ipv4 =
  let doc = "The IP address of the unikernel." in
  let ipaddr = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let doc = "The IP gateway." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let servers =
  let doc = "IPv4 of NTP servers." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt_all ipaddr [] & info [ "server" ] ~doc ~docv:"IPv4"

let term =
  let open Term in
  const run $ setup_logs $ ipv4 $ ipv4_gateway $ servers

let cmd =
  let info = Cmd.info "chaos" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
