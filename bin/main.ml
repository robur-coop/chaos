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
  val idle : t -> unit
  val interrupt : t -> unit
end = struct
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
    | Some c -> t.waiter <- None; ignore (Miou.Computation.try_return c ())
    | None -> t.pending <- true

  let idle t =
    if t.pending then t.pending <- false
    else begin
      let c = Miou.Computation.create () in
      t.waiter <- Some c;
      Miou.Computation.await_exn c
    end
end

let when_ntpv4_is_received _trigger ts wk =
  ts := Chaos.Clock.read_cooked_time ();
  Wk.interrupt wk

let new_listener orphans udp actives wk port =
  match Hashtbl.find_opt actives port with
  | Some _ -> ()
  | None ->
      let trigger = Miou.Trigger.create () in
      let ts = ref Ptime.min in
      assert (Miou.Trigger.on_signal trigger ts wk when_ntpv4_is_received);
      let prm =
        Miou.async ~orphans @@ fun () ->
        let buf = Bytes.create 0x7ff in
        let len, (peer, _peer_port) = Mnet.UDP.recvfrom udp ~port ~trigger buf in
        let str = Bytes.sub_string buf 0 len in
        match Chaos.Packet.decode str with
        | Ok pkt -> `Packet (!ts, pkt, peer, port)
        | Error _ -> `Unknown port
      in
      Hashtbl.add actives port prm

let cancel_listener active_ports port prm =
  if List.exists (Int.equal port) active_ports = false then
    let () = Miou.cancel prm in
    None
  else Some prm

let clean_up_listeners udpv4 orphans rxs actives wk =
  let rxs = List.filter Chaos.Source.rx_active rxs in
  let active_ports = List.map Chaos.Source.rx_port rxs in
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
            List.iter (Chaos.Source.rx_received ~src ~src_port ~ts pkt) rxs
        | Ok (`Unknown port) -> Hashtbl.remove actives port
        | Error Miou.Cancelled -> ()
        | Error exn ->
            Logs.err (fun m ->
                m "A listener terminated with an exception: %s"
                  (Printexc.to_string exn))
      in
      rxs

let rec step udp wk sleepers rxs server =
  match Chaos.Source.handle server with
  | `Send (src_port, pkt, tx, rx) ->
      let dst, port = Chaos.Source.server server in
      let _ =
        Miou.async ~orphans:sleepers @@ fun () ->
        Mkernel.sleep 3_000_000_000;
        Chaos.Source.rx_timeout rx;
        Wk.interrupt wk
      in
      let ts = ref Ptime.min in
      let ok _ = Chaos.Source.tx_sent tx !ts
      and error _ =
        Logs.warn (fun m -> m "%a:%d unreachable" Ipaddr.pp dst port);
        Chaos.Source.dst_unreachable tx
      in
      let now () = Chaos.Clock.read_cooked_time () in
      (* NOTE(dinosaure): [fn] is executed **after** the discovery
         of routes. When the new NTPv4 packet is sent, we have the most accurate
         time of transmission from the perspective of the unikernel. *)
      let fn bstr = ts := Chaos.Packet.encode_into ~now pkt bstr in
      Mnet.UDP.sendfn udp ~src_port ~dst ~port ~len:48 fn
      |> Result.fold ~ok ~error;
      Wk.interrupt wk;
      step udp wk sleepers (rx :: rxs) server
  | `Await -> `Continue (rxs, server)
  | `Error _ -> `Stop rxs
  | `Sleep (sleeper, ns) ->
      let _ =
        Miou.async ~orphans:sleepers @@ fun () ->
        Mkernel.sleep ns;
        Chaos.Source.wake_up sleeper;
        Wk.interrupt wk
      in
      step udp wk sleepers rxs server

let run metrics udpv4 servers =
  let _ = Chaos.Clock.init Tscclock.now in
  let wk = Wk.create () in
  let actives = Hashtbl.create 0x10 in
  let reference = Chaos.Reference.make ?logs:metrics () in
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
      | servers ->
          let now = Chaos.Clock.read_cooked_time () in
          (* TODO(dinosaure): for [now], get monotonic last event time *)
          let res = Chaos.Select.select now servers in
          let fn (source, data, combined_sources, leap) =
            let server = Chaos.Source.server source in
            let stratum = Chaos.Source.stratum source in
            Chaos.Reference.update reference ~stratum ~combined_sources ~leap
              server data
          in
          Option.iter fn res;
          Wk.idle wk;
          go rxs (List.rev servers)
    in
    go [] (List.map Chaos.Source.make servers)
  in
  Miou.await_exn prm

module RNG = Mirage_crypto_rng.Fortuna

module Append = struct
  type t = { tmp: Bstr.t; hdr: Bstr.t; mutable abs: int; mutable pos: int }

  let make blk =
    let pagesize = Mkernel.Block.pagesize blk in
    { tmp= Bstr.create pagesize; hdr= Bstr.create pagesize; pos= 8; abs= 0 }

  let next t =
    t.pos <- 0;
    Bstr.fill t.tmp ~off:0 '\000';
    t.abs <- t.abs + 1

  let set_header t blk real_length =
    Mkernel.Block.atomic_read blk ~src_off:0 ~dst_off:0 t.hdr;
    Bstr.set_int64_be t.hdr 0 (Int64.of_int real_length);
    Mkernel.Block.atomic_write blk ~src_off:0 ~dst_off:0 t.hdr

  let rec flush blk t = function
    | _, _, 0 -> ()
    | str, off, len ->
        let to_write = Int.min len (Bstr.length t.tmp - t.pos) in
        let src_off = off and dst_off = t.pos in
        Bstr.blit_from_string str ~src_off t.tmp ~dst_off ~len:to_write;
        t.pos <- t.pos + to_write;
        let dst_off = t.abs * Bstr.length t.tmp in
        Mkernel.Block.atomic_write blk ~src_off:0 ~dst_off t.tmp;
        let real_length = (t.abs * Bstr.length t.tmp) + t.pos in
        set_header t blk real_length;
        if t.pos = Bstr.length t.tmp then next t;
        flush blk t (str, off + to_write, len - to_write)

  let append blk t str off len = flush blk t (str, off, len)

  let format_of_block_device blk =
    let t = make blk in
    let out_string = append blk t in
    let out_flush = ignore in
    let out_width _str ~pos:_ ~len = len in
    let newline = ("\n", 0, 1) in
    let out_newline () = flush blk t newline in
    let out_spaces len = flush blk t (String.make len '\x20', 0, len) in
    let out_indent len = flush blk t (String.make len '\x09', 0, len) in
    Format.formatter_of_out_functions
      { out_string; out_flush; out_newline; out_spaces; out_indent; out_width }

  let of_block name =
    let fn blk () = Some (format_of_block_device blk) in
    Mkernel.map fn Mkernel.[ block name ]
end

let run _ (cidr, gateway, ipv6) metrics servers =
  let metrics =
    match metrics with
    | None -> Mkernel.const None
    | Some name -> Append.of_block name
  in
  Mkernel.(run [ Mnet.stack ~name:"service" ?gateway ~ipv6 cidr; metrics ])
  @@ fun (daemon, _tcpv4, udpv4) metrics () ->
  let rng = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let finally () =
    Mirage_crypto_rng_mkernel.kill rng;
    Mnet.kill daemon
  in
  Fun.protect ~finally @@ fun () ->
  let _ = Tscclock.init () in
  run metrics udpv4 servers

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
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
      let t1 = Mkernel.clock_monotonic () in
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

let metrics =
  let doc = "Save metrics into the given block device." in
  let open Arg in
  value & opt (some string) None & info [ "metrics" ] ~doc ~docv:"NAME"

let servers =
  let doc = "IPv4 of NTP servers." in
  let ipaddr = Arg.conv (Ipaddr.of_string, Ipaddr.pp) in
  let open Arg in
  value & opt_all ipaddr [] & info [ "server" ] ~doc ~docv:"IPv4"

let term =
  let open Term in
  const run $ setup_logs $ Mnet_cli.setup $ metrics $ servers

let cmd =
  let info = Cmd.info "chaos" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
