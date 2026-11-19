let ( let@ ) finally fn = Fun.protect ~finally fn
let ( let* ) = Result.bind
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let guard ~err fn = if fn () then Ok () else Error err
let _RESOLVE_INTERVAL = 60.0
let _COMPACT_INTERVAL = 3600.0

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

let handler ~orphans keys udp srv reference raw rx peer peer_port =
  try
    let auth = Chaos.Auth.check keys raw in
    let pkt = Chaos.Packet.decode raw in
    let fn req =
      match Chaos.Server.handle srv reference ~auth ~rx ~peer req with
      | Some (resp, sign) ->
          let sign = Option.bind sign (Chaos.Auth.find keys) in
          let len = Option.map (fun _ -> 48 + Chaos.Auth.mac_length) sign in
          let len = Option.value ~default:48 len in
          let fn () =
            let now () = Chaos.Clock.read_cooked_time () in
            let fn bstr =
              ignore (Chaos.Packet.encode_into ~now resp bstr);
              Option.iter (fun k -> Chaos.Auth.append_into k bstr) sign
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

let[@inline always] compact last_compact = function
  | [] ->
      let raw = Chaos.Clock.read_raw_time () in
      if Ptime.(Span.to_float_s (diff raw !last_compact)) > _COMPACT_INTERVAL
      then begin
        Gc.compact ();
        last_compact := Chaos.Clock.read_raw_time ()
      end
  | _ -> ()

type origin = [ `Ipaddr of Ipaddr.t | `Domain_name of [ `host ] Domain_name.t ]

and pool = {
    origin: origin
  ; mutable sources: Chaos.Source.t list
  ; mutable last_resolve: Ptime.t
}

let is_domain_pool pool =
  match pool.origin with `Domain_name _ -> true | `Ipaddr _ -> false

let resolve dns range ckey locals now pool =
  match pool.origin with
  | `Ipaddr ipaddr ->
      if pool.sources = [] then
        pool.sources <- [ Chaos.Source.make ?key:ckey ~locals ipaddr ]
  | `Domain_name domain_name ->
      let lo, hi = range in
      let n = List.length pool.sources in
      let due =
        Ptime.(Span.to_float_s (diff now pool.last_resolve))
        >= _RESOLVE_INTERVAL
      in
      if n < lo && due then begin
        pool.last_resolve <- now;
        match Mnet_dns.getaddrinfo dns Dns.Rr_map.A domain_name with
        | Ok (_ttl, set) ->
            let existing =
              List.map (fun s -> fst (Chaos.Source.server s)) pool.sources
            in
            let fresh =
              Ipaddr.V4.Set.elements set
              |> List.filter_map (fun v4 ->
                  let ip = Ipaddr.V4 v4 in
                  if List.mem ip existing then None else Some ip)
            in
            let chosen = List.filteri (fun i _ -> i < hi - n) fresh in
            if chosen <> [] then
              Logs.info (fun m ->
                  m "%a: adding %d source(s) from re-resolution" Domain_name.pp
                    domain_name (List.length chosen));
            let added =
              List.map (fun ip -> Chaos.Source.make ?key:ckey ~locals ip) chosen
            in
            pool.sources <- pool.sources @ added
        | Error (`Msg msg) ->
            Logs.warn (fun m ->
                m "Cannot resolve %a: %s" Domain_name.pp domain_name msg)
      end

let run dns range keyspecs ckey locals udp servers =
  let _ = Chaos.Clock.init Tscclock.now in
  let wk = Mchaos.waker () in
  let last_compact = ref (Chaos.Clock.read_raw_time ()) in
  let reference = Chaos.Reference.make () in
  let srv = Chaos.Server.make () in
  let keys = Chaos.Auth.make keyspecs in
  let ckey = Option.bind ckey (Chaos.Auth.find keys) in
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
  let pools =
    List.map
      (fun origin -> { origin; sources= []; last_resolve= Ptime.min })
      servers
  in
  let sources () = List.concat_map (fun pool -> pool.sources) pools in
  Metrics.register ~reference ~server:srv sources;
  let prm1 =
    Miou.async @@ fun () ->
    let sleepers = Miou.orphans () in
    let listeners = Mchaos.listeners () in
    let step_pool rxs pool =
      let fn (sources, rxs) source =
        match Mchaos.step udp wk sleepers rxs source with
        | `Continue, rxs -> (source :: sources, rxs)
        | `Stop, rxs -> (sources, rxs)
      in
      let sources, rxs = List.fold_left fn ([], rxs) pool.sources in
      pool.sources <- List.rev sources;
      rxs
    in
    let rec go rxs =
      clean_up sleepers;
      let now = Chaos.Clock.read_cooked_time () in
      List.iter (resolve dns range ckey locals now) pools;
      let rxs = List.fold_left step_pool rxs pools in
      let rxs = Mchaos.listen keys udp wk listeners rxs in
      let servers = List.concat_map (fun pool -> pool.sources) pools in
      match servers with
      | [] when not (List.exists is_domain_pool pools) ->
          Mchaos.kill listeners; terminate sleepers
      | [] ->
          let _ =
            Miou.async ~orphans:sleepers @@ fun () ->
            Mkernel.sleep (int_of_float (_RESOLVE_INTERVAL *. 1e9));
            Mchaos.interrupt wk
          in
          Mchaos.idle wk; go rxs
      | servers ->
          let now = Chaos.Clock.read_cooked_time () in
          let res = Chaos.Select.select now servers in
          let fn (source, data, combined_sources, leap) =
            let server = Chaos.Source.server source in
            let stratum = Chaos.Source.stratum source in
            Chaos.Reference.update reference ~stratum ~combined_sources ~leap
              server data
          in
          Option.iter fn res;
          (compact [@inlined]) last_compact rxs;
          Mchaos.idle wk;
          go rxs
    in
    go []
  in
  let _ = Miou.await_all [ prm0; prm1 ] in
  ()

module RNG = Mirage_crypto_rng.Fortuna

let kill_tcp_if_possible ~nameservers:(proto, _) stack hed =
  match proto with
  | `Udp ->
      Mnet_happy_eyeballs.kill hed;
      Mnet.TCP.kill (Mnet.tcp stack)
  | `Tcp -> ()

let happy_eyeballs cfg =
  let {
    Mnet_happy_eyeballs_cli.aaaa_timeout
  ; connect_delay
  ; connect_timeout
  ; resolve_timeout
  ; resolve_retries
  } =
    cfg
  in
  let now = Int64.of_int (Mkernel.clock_monotonic ()) in
  Happy_eyeballs.create ~aaaa_timeout ~connect_delay ~connect_timeout
    ~resolve_timeout ~resolve_retries now

let run _ service cfg nameservers keys ckey servers range public metrics =
  let happy_eyeballs = happy_eyeballs cfg in
  Mkernel.(run [ service; metrics ]) @@ fun (stack, tcp, udp) _metrics () ->
  let rng = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  let@ () = fun () -> Mnet.kill stack in
  let hed, he = Mnet_happy_eyeballs.create ~happy_eyeballs tcp in
  let@ () = fun () -> Mnet_happy_eyeballs.kill hed in
  let dns_stack = Mnet_dns.Transport.stack udp he in
  let dns = Mnet_dns.create ~nameservers dns_stack in
  let@ () = fun () -> Mnet_dns.Transport.kill (Mnet_dns.transport dns) in
  (* NOTE(dinosaure): if our nameservers use only UDP nameservers, we can safely
     kill our TCP daemon and our happy-eyeballs daemon to save some works. This
     also means that [Mnet_happy_eyeballs.connect] will no longer work. *)
  kill_tcp_if_possible ~nameservers stack hed;
  let _ = Tscclock.init () in
  let locals =
    List.filter_map
      (function Ipaddr.V4 v4 -> Some v4 | Ipaddr.V6 _ -> None)
      (List.map Ipaddr.Prefix.address (Mnet.addresses stack))
  in
  let locals = List.sort_uniq Ipaddr.V4.compare (public @ locals) in
  run dns range keys ckey locals udp servers

open Cmdliner

let range =
  let doc = "Number of servers (IP addresses) that Chaos uses as a source." in
  let parser str =
    match String.split_on_char ',' str with
    | [ a; b ] ->
        let none = msgf "Invalid range number" in
        let* a = int_of_string_opt a |> Option.to_result ~none in
        let* b = int_of_string_opt b |> Option.to_result ~none in
        let err = msgf "Number must be positive" in
        let* () = guard ~err @@ fun () -> a > 0 in
        let* () = guard ~err @@ fun () -> b > 0 in
        let* () = guard ~err:(msgf "Reverse range") @@ fun () -> a <= b in
        Ok (a, b)
    | _ -> error_msgf "Invalid range: %S" str
  in
  let pp ppf (a, b) = Fmt.pf ppf "%d,%d" a b in
  let range = Arg.conv (parser, pp) in
  let open Arg in
  value & opt range (2, 4) & info [ "r"; "range" ] ~doc ~docv:"A,B"

let servers =
  let doc = "NTP servers." in
  let parser str =
    match Ipaddr.of_string str with
    | Ok ipaddr -> Ok (`Ipaddr ipaddr)
    | Error _ ->
        let* dn = Domain_name.of_string str in
        let* dn = Domain_name.host dn in
        Ok (`Domain_name dn)
  in
  let pp ppf = function
    | `Ipaddr ipaddr -> Ipaddr.pp ppf ipaddr
    | `Domain_name domain_name -> Domain_name.pp ppf domain_name
  in
  let server = Arg.conv (parser, pp) in
  let open Arg in
  value & opt_all server [] & info [ "server" ] ~doc ~docv:"SERVER"

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
    "Identifier of the key used to authenticate requests to upstream servers."
  in
  let open Arg in
  value & opt (some int) None & info [ "client-key" ] ~doc ~docv:"ID"

let public =
  let doc =
    "A public IPv4 address which designates this unikernel (for instance when \
     it is behind a NAT). Replies from servers synchronised to one of these \
     addresses (or to the address of the service interface) are discarded to \
     avoid a synchronisation loop. This option can be repeated."
  in
  let ipv4 = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt_all ipv4 [] & info [ "public-ipv4" ] ~doc ~docv:"IPV4"

let docs_metrics = "METRICS"

let metrics_ipv4 =
  let doc =
    "The IPv4 address (with its prefix) of the metrics interface. If it is not \
     specified (and a metrics destination is given), the metrics interface is \
     configured via a DHCP server."
  in
  let cidr4 = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  value
  & opt (some cidr4) None
  & info [ "metrics-ipv4" ] ~doc ~docs:docs_metrics ~docv:"CIDRV4"

let metrics_ipv4_gateway =
  let doc = "The IPv4 gateway of the metrics interface." in
  let gateway4 = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value
  & opt (some gateway4) None
  & info [ "metrics-ipv4-gateway" ] ~doc ~docs:docs_metrics ~docv:"IPV4"

let metrics =
  let doc =
    "The address of the Telegraf server which collects metrics. If it is not \
     specified, metrics are not reported."
  in
  let pp ppf (ipaddr, port) = Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port in
  let addr = Arg.conv (Ipaddr.with_port_of_string ~default:8094, pp) in
  let open Arg in
  value
  & opt (some addr) None
  & info [ "metrics" ] ~doc ~docs:docs_metrics ~docv:"IPADDR"

let name ~default =
  let doc = "The name of the unikernel." in
  let open Arg in
  value
  & opt string default
  & info [ "name" ] ~doc ~docs:docs_metrics ~docv:"NAME"

let setup_metrics ipv4 gateway dst name =
  let cfg = Option.map (fun ipv4 -> (ipv4, gateway)) ipv4 in
  let dst, port =
    match dst with
    | Some (dst, port) -> (Some dst, Some port)
    | None -> (None, None)
  in
  Tally_mnet.device ~device:"metrics" ~name cfg ?port dst

let setup_metrics ~default =
  let open Term in
  const setup_metrics
  $ metrics_ipv4
  $ metrics_ipv4_gateway
  $ metrics
  $ name ~default

let term =
  let open Term in
  const run
  $ Mnet_cli.setup_logs
  $ Mnet_cli.setup "service"
  $ Mnet_happy_eyeballs_cli.setup
  $ Mnet_dns_cli.setup ()
  $ keys
  $ ckey
  $ servers
  $ range
  $ public
  $ setup_metrics ~default:"chaos"

let cmd =
  let info = Cmd.info "chaos" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
