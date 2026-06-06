let () = Printexc.record_backtrace true
let ( let@ ) finally fn = Fun.protect ~finally fn

let rec clean_up_sleepers orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> ()
  | Some (Some prm) ->
      begin match Miou.await prm with
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
  | Some (Some prm) ->
      begin match Miou.await prm with
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

let when_ntp_is_received _trigger ts wk =
  ts := Chaos.Clock.read_cooked_time ();
  Wk.interrupt wk

let new_listener keys orphans udp actives wk port =
  match Hashtbl.find_opt actives port with
  | Some _ -> ()
  | None ->
      let trigger = Miou.Trigger.create () in
      let ts = ref Ptime.min in
      assert (Miou.Trigger.on_signal trigger ts wk when_ntp_is_received);
      let prm =
        Miou.async ~orphans @@ fun () ->
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
      Hashtbl.add actives port prm

let cancel_listener active_ports port prm =
  if List.exists (Int.equal port) active_ports = false then
    let () = Miou.cancel prm in
    None
  else Some prm

let clean_up_listeners keys udpv4 orphans rxs actives wk =
  let rxs = List.filter Chaos.Source.rx_active rxs in
  let active_ports = List.map Chaos.Source.rx_port rxs in
  let active_ports = List.sort_uniq Int.compare active_ports in
  Hashtbl.filter_map_inplace (cancel_listener active_ports) actives;
  let new_ports = List.filter (Fun.negate (Hashtbl.mem actives)) active_ports in
  List.iter (new_listener keys orphans udpv4 actives wk) new_ports;
  match Miou.care orphans with
  | None | Some None -> rxs
  | Some (Some prm) ->
      let () =
        match Miou.await prm with
        | Ok (`Packet (ts, pkt, auth, src, src_port)) ->
            Hashtbl.remove actives src_port;
            List.iter
              (Chaos.Source.rx_received ~src ~src_port ~ts ~auth pkt)
              rxs
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
      let key = Chaos.Source.key server in
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

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      begin match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error exn ->
          Logs.err (fun m ->
              m "Unexpected exception from a task: %s" (Printexc.to_string exn));
          clean_up orphans
      end

let handler ~orphans keys udp srv reference raw rx peer peer_port =
  try
    let auth = Chaos.Auth.check keys raw in
    let pkt = Chaos.Packet.decode raw in
    let fn req =
      match Chaos.Server.handle srv reference ~auth ~rx ~peer req with
      | Some (resp, sign) ->
          (* Sign the response with the request's key when it was authenticated. *)
          let sign_key = Option.bind sign (Chaos.Auth.find keys) in
          let len =
            match sign_key with
            | None -> 48
            | Some _ -> 48 + Chaos.Auth.mac_length
          in
          let fn () =
            let now () = Chaos.Clock.read_cooked_time () in
            let fn bstr =
              ignore (Chaos.Packet.encode_into ~now resp bstr);
              Option.iter (fun k -> Chaos.Auth.append_into k bstr) sign_key
            in
            Mnet.UDP.sendfn udp ~src_port:123 ~dst:peer ~port:peer_port ~len fn
            |> ignore
          in
          ignore (Miou.async ~orphans fn)
      | None -> ()
    in
    Result.iter fn pkt
  with exn ->
    Logs.warn (fun m ->
        m "Discarding a request from %a: %s" Ipaddr.pp peer
          (Printexc.to_string exn))

(* How often to compact the heap (and return memory to the host) to keep the
   footprint bounded over a multi-year uptime. *)
let _COMPACT_INTERVAL = 3600.0

let[@inline always] compact last_compact = function
  | [] ->
      let raw = Chaos.Clock.read_raw_time () in
      if Ptime.(Span.to_float_s (diff raw !last_compact)) > _COMPACT_INTERVAL
      then begin
        Gc.compact ();
        last_compact := Chaos.Clock.read_raw_time ()
      end
  | _ -> ()

let run metrics keyspecs ckey udp servers =
  let _ = Chaos.Clock.init Tscclock.now in
  let wk = Wk.create () in
  let last_compact = ref (Chaos.Clock.read_raw_time ()) in
  let actives = Hashtbl.create 0x10 in
  let reference = Chaos.Reference.make ?logs:metrics () in
  let srv = Chaos.Server.make () in
  let keys = Chaos.Auth.make keyspecs in
  (* Key the client uses to authenticate all its upstream requests, if any. *)
  let ckey = Option.bind ckey (Chaos.Auth.find keys) in
  (* NTP server: always listening on UDP/123, answering client requests from the
     reference state maintained by the client loop below. *)
  let prm0 =
    Miou.async @@ fun () ->
    let buf = Bytes.create 0x7ff in
    let rec serve orphans =
      clean_up orphans;
      let trigger = Miou.Trigger.create () in
      let rx = ref Ptime.min in
      assert (
        Miou.Trigger.on_signal trigger rx () @@ fun _trigger rx () ->
        rx := Chaos.Clock.read_cooked_time ());
      let len, (peer, peer_port) =
        Mnet.UDP.recvfrom udp ~trigger ~port:123 buf
      in
      let pkt = Bytes.sub_string buf 0 len in
      handler ~orphans keys udp srv reference pkt !rx peer peer_port;
      serve orphans
    in
    serve (Miou.orphans ())
  in
  let prm1 =
    Miou.async @@ fun () ->
    let sleepers = Miou.orphans () in
    let listeners = Miou.orphans () in
    let rec go rxs servers =
      clean_up_sleepers sleepers;
      let fn (servers, rxs) server =
        match step udp wk sleepers [] server with
        | `Continue ([], server) -> (server :: servers, rxs)
        | `Continue (rxs', server) ->
            (server :: servers, List.rev_append rxs' rxs)
        | `Stop [] -> (servers, rxs)
        | `Stop rxs' -> (servers, List.rev_append rxs' rxs)
      in
      let servers, rxs = List.fold_left fn ([], rxs) servers in
      let rxs = clean_up_listeners keys udp listeners rxs actives wk in
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
          (compact [@inlined]) last_compact rxs;
          Wk.idle wk;
          go rxs (List.rev servers)
    in
    go [] (List.map (Chaos.Source.make ?key:ckey) servers)
  in
  let _ = Miou.await_all [ prm0; prm1 ] in
  ()

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

let devices (cidr, gateway, ipv6) metrics =
  let metrics =
    match metrics with
    | None -> Mkernel.const None
    | Some name -> Append.of_block name
  in
  let open Mkernel in
  [
    Mnet.stack ~name:"service" ?gateway ~ipv6 cidr; metrics
  ; Mkernel_memtrace.block "memtrace"
  ]

let run _ mnet metrics keys ckey servers =
  Mkernel.run (devices mnet metrics)
  @@ fun (daemon, _tcpv4, udp) metrics _memtrace () ->
  let rng = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  let@ () = fun () -> Mnet.kill daemon in
  let _ = Tscclock.init () in
  run metrics keys ckey udp servers

open Cmdliner

let metrics =
  let doc = "Save metrics into the given block device." in
  let open Arg in
  value & opt (some string) None & info [ "metrics" ] ~doc ~docv:"NAME"

let servers =
  let doc = "IPv4 of NTP servers." in
  let ipaddr = Arg.conv (Ipaddr.of_string, Ipaddr.pp) in
  let open Arg in
  value & opt_all ipaddr [] & info [ "server" ] ~doc ~docv:"IPv4"

let keys =
  let doc =
    "A symmetric authentication key, as ID:ALGO:HEX (ALGO is SHA1 or SHA256)."
  in
  let pp ppf (k : Chaos.Auth.key) = Fmt.pf ppf "%d" k.Chaos.Auth.id in
  let key = Arg.conv (Chaos.Auth.of_cli, pp) in
  let open Arg in
  value & opt_all key [] & info [ "key" ] ~doc ~docv:"ID:ALGO:HEX"

let ckey =
  let doc =
    "Identifier of the key used to authenticate requests to all upstream \
     servers."
  in
  let open Arg in
  value & opt (some int) None & info [ "client-key" ] ~doc ~docv:"ID"

let term =
  let open Term in
  const run
  $ Mnet_cli.setup_logs
  $ Mnet_cli.setup
  $ metrics
  $ keys
  $ ckey
  $ servers

let cmd =
  let info = Cmd.info "chaos" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
