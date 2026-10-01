let ( let@ ) finally fn = Fun.protect ~finally fn
let _RETRY_INTERVAL = 60_000_000_000 (* see mchaos.ml *)
let src = Logs.Src.create "front"
let strf = Fmt.str
let bpf buf fmt = Printf.bprintf buf fmt

module Log = (val Logs.src_log src : Logs.LOG)
module RNG = Mirage_crypto_rng.Fortuna

let rng =
  let fn () = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let finally = Mirage_crypto_rng_mkernel.kill in
  Mkernel.map fn Mkernel.[] |> Mkernel.finally finally

let pp_origin ppf = function
  | `Ipaddr ipaddr -> Ipaddr.pp ppf ipaddr
  | `Domain_name domain_name -> Domain_name.pp ppf domain_name

let style =
  {css|
body {
  font-family: monospace;
  color: #333;
  margin: 5% 0 5% 15%;
  width: min(720px, 60%);
  line-height: 1.6;
}
a, a:visited { color: #333; text-decoration: none; font-weight: bold; }
a:hover { text-decoration: underline; }
h1 { margin-bottom: 0.2em; }
h1 small { font-weight: normal; font-size: 0.5em; color: #888; }
h2 { margin-top: 2em; }
code { background: #f4f4f4; padding: 0.1em 0.3em; }
pre { background: #f4f4f4; padding: 1em; overflow-x: auto; }
.now { font-size: 1.6em; font-weight: bold; margin: 0.5em 0 0 0; }
.sub { color: #888; margin: 0; }
table { border-collapse: collapse; width: 100%; }
td { padding: 0.1em 1em 0.1em 0; vertical-align: top; }
td:first-child { white-space: nowrap; }
td.v { white-space: nowrap; font-weight: bold; }
td.d { color: #888; }
.ok { color: #2a7a2a; }
.ko { color: #b03030; }
@media (max-width: 700px) {
  body { margin: 5%; width: 90%; }
  td.d { display: none; }
}
|css}

let escape str =
  let buf = Buffer.create (String.length str) in
  let fn = function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | chr -> Buffer.add_char buf chr
  in
  String.iter fn str; Buffer.contents buf

let pp_duration ~sign ppf v =
  let a = Float.abs v in
  let sign = if sign && v > 0.0 then "+" else "" in
  if Float.is_nan v then Fmt.string ppf "-"
  else if a = 0.0 then Fmt.string ppf "0 s"
  else if a < 1e-6 then Fmt.pf ppf "%s%.3f ns" sign (v *. 1e9)
  else if a < 1e-3 then Fmt.pf ppf "%s%.3f µs" sign (v *. 1e6)
  else if a < 1.0 then Fmt.pf ppf "%s%.3f ms" sign (v *. 1e3)
  else Fmt.pf ppf "%s%.6f s" sign v

let pp_seconds = pp_duration ~sign:true
let pp_delay = pp_duration ~sign:false

let pp_ago ppf v =
  let v = Float.abs v in
  if v < 60.0 then Fmt.pf ppf "%.1fs ago" v
  else if v < 3600.0 then
    Fmt.pf ppf "%dm%02ds ago" (truncate v / 60) (truncate v mod 60)
  else Fmt.pf ppf "%dh%02dm ago" (truncate v / 3600) (truncate v mod 3600 / 60)

let pp_uptime ppf v =
  let v = truncate v in
  let d = v / 86400 and h = v mod 86400 / 3600 in
  let m = v mod 3600 / 60 and s = v mod 60 in
  if d > 0 then Fmt.pf ppf "%dd %02dh%02dm%02ds" d h m s
  else Fmt.pf ppf "%02dh%02dm%02ds" h m s

let pp_ptime = Ptime.pp_rfc3339 ~frac_s:9 ~tz_offset_s:0 ()
let pp_ppm ppf v = Fmt.pf ppf "%+.3f ppm" v

let pp_leap ppf = function
  | 0 -> Fmt.string ppf "normal"
  | 1 -> Fmt.string ppf "insert second"
  | 2 -> Fmt.string ppf "delete second"
  | _ -> Fmt.string ppf "not synchronised"

let pp_ref_id ppf ref_id =
  Fmt.pf ppf "%08X (%d.%d.%d.%d)" ref_id
    ((ref_id lsr 24) land 0xff)
    ((ref_id lsr 16) land 0xff)
    ((ref_id lsr 8) land 0xff)
    (ref_id land 0xff)

let pp_reach ppf (bits, size) =
  let str =
    String.init size (fun i ->
        if bits land (1 lsl (size - 1 - i)) <> 0 then '1' else '0')
  in
  Fmt.pf ppf "%03o (%s)" bits (if size = 0 then "-" else str)

(* Like the first column of [chronyc sources]. *)
let source_state source =
  if not (Chaos.Source.is_reachable source) then ("?", "unreachable")
  else if Chaos.Source.is_falseticker source then ("x", "falseticker")
  else if Chaos.Source.selected source then ("*", "selected")
  else ("-", "not selected")

let section buf title rows =
  bpf buf "<h2>%s</h2>\n<table>\n" title;
  let fn (k, v, d) =
    bpf buf
      "<tr><td>%s</td><td class=\"v\">%s</td><td class=\"d\">%s</td></tr>\n" k
      (escape v) d
  in
  List.iter fn rows; bpf buf "</table>\n"

let render state =
  let raw = Chaos.Clock.read_raw_time () in
  let now = Chaos.Clock.cook raw in
  let t = Chaos.Reference.tracking state.Mchaos.reference now in
  let origin, port = state.server in
  let server = escape (strf "%a:%d" pp_origin origin port) in
  let buf = Buffer.create 0x4000 in
  bpf buf
    {html|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="refresh" content="10">
  <title>chaos - what time is it?</title>
  <style>%s</style>
</head>
<body>
  <h1>chaos <small>what time is it?</small></h1>
  <p class="now" id="now">%s</p>
  <p class="sub">raw clock: %s<br>
  %s</p>
|html}
    style (strf "%a" pp_ptime now) (strf "%a" pp_ptime raw)
    (if t.Chaos.Reference.synchronised then
       strf "<span class=\"ok\">synchronised</span> with %s" server
     else strf "<span class=\"ko\">not (yet) synchronised</span> with %s" server);
  bpf buf
    {html|
  <p>This page is served by a <a href="https://uniker.nl/">unikernel</a>
  which disciplines its own clock with
  <a href="https://github.com/robur-coop/chaos">chaos</a>, an NTP
  implementation in <a href="https://ocaml.org/">OCaml</a> inspired by
  <a href="https://chrony-project.org/">chrony</a>. It never trusts the
  hardware clock (the <em>raw</em> time): it continuously asks the NTP server
  <code>%s</code> for the time, estimates the offset and the drift of its
  own oscillator and serves you the <em>cooked</em> (corrected) time. The
  page refreshes itself every 10 seconds.</p>
|html}
    server;
  let update_age =
    match t.last_ref_time with
    | Some ref_time ->
        strf "%a" pp_ago Ptime.(Span.to_float_s (diff now ref_time))
    | None -> "never"
  in
  let ref_time =
    match t.last_ref_time with
    | Some ref_time -> strf "%a" pp_ptime ref_time
    | None -> "-"
  in
  section buf "Tracking"
    [
      ( "Reference ID"
      , (if t.synchronised then strf "%a" pp_ref_id t.ref_id else "-")
      , "the NTP identifier of our reference (the IPv4 address of the server)"
      )
    ; ( "Stratum"
      , string_of_int t.stratum
      , "distance (in hops) from a reference clock; 0 means unsynchronised" )
    ; ("Ref time (UTC)", ref_time, "when the last measurement was processed")
    ; ("Last update", update_age, "")
    ; ( "System time"
      , strf "%a" pp_seconds (Chaos.Clock.pending_correction raw)
      , "correction still to apply (slewing) to the raw clock" )
    ; ( "Last offset"
      , strf "%a" pp_seconds t.last_offset
      , "offset estimated on the last clock update" )
    ; ( "RMS offset"
      , strf "%a" pp_seconds t.rms_offset
      , "long-term average of the offset" )
    ; ( "Frequency"
      , strf "%a" pp_ppm (Chaos.Clock.frequency ())
      , "rate at which our oscillator would be wrong without correction" )
    ; ( "Residual freq"
      , strf "%a" pp_ppm (t.residual_freq *. 1e6)
      , "frequency difference between the reference and the current one" )
    ; ( "Skew"
      , strf "%.3f ppm" (t.skew *. 1e6)
      , "estimated error bound on the frequency" )
    ; ( "Root delay"
      , strf "%a" pp_delay t.root_delay
      , "total round-trip delay to the stratum 1 computer" )
    ; ( "Root dispersion"
      , strf "%a" pp_delay t.root_dispersion
      , "total dispersion accumulated through all computers" )
    ; ("Offset std dev", strf "%a" pp_delay t.offset_sd, "")
    ; ("Frequency std dev", strf "%a" pp_ppm (t.frequency_sd *. 1e6), "")
    ; ("Leap status", strf "%a" pp_leap t.leap, "")
    ; ( "Updates"
      , strf "%d (%d rejected)" t.updates t.rejected_updates
      , "number of clock updates" )
    ; ("Combined sources", string_of_int t.combined_sources, "")
    ];
  section buf "Clock"
    [
      ( "Raw time (UTC)"
      , strf "%a" pp_ptime raw
      , "the TSC-based clock of the unikernel, never corrected" )
    ; ( "Cooked time (UTC)"
      , strf "%a" pp_ptime now
      , "the raw time with all accumulated corrections" )
    ; ( "Difference"
      , strf "%a" pp_seconds Ptime.(Span.to_float_s (diff now raw))
      , "" )
    ; ( "Precision"
      , strf "2^%d s (%a)"
          (Chaos.Clock.precision_as_log ())
          pp_delay
          (Chaos.Clock.precision_as_quantum ())
      , "the time needed to read the clock" )
    ; ( "Uptime"
      , strf "%a" pp_uptime Ptime.(Span.to_float_s (diff raw state.started))
      , "" )
    ];
  begin match state.source with
  | None ->
      section buf "Source"
        [
          ( "State"
          , "dropped"
          , strf "chaos will retry in %ds" (_RETRY_INTERVAL / 1_000_000_000) )
        ]
  | Some source ->
      let open Chaos.Source in
      let st, descr = source_state source in
      let counters = counters source in
      let stats = stats source in
      let reach = (reachability_bits source, reachability_size source) in
      let rows =
        [
          ("Server", strf "%a" pp_origin origin, "")
        ; ( "Address"
          , (let ipaddr, port = Chaos.Source.server source in
             strf "%a:%d" Ipaddr.pp ipaddr port)
          , "the IP address currently used (resolved via DNS)" )
        ; ("State", strf "%s %s" st descr, "")
        ; ("Stratum", string_of_int (stratum source), "")
        ; ("Leap", strf "%a" pp_leap (leap source), "")
        ; ( "Reach"
          , strf "%a" pp_reach reach
          , "the last 8 requests, 1 if a valid reply was received" )
        ; ( "Poll"
          , strf "2^%d s (%ds)" (local_poll source) (1 lsl local_poll source)
          , "interval between two requests" )
        ; ( "Remote poll"
          , Option.fold ~none:"-" ~some:(strf "2^%d s") (remote_poll source)
          , "" ); ("Score", strf "%.3f" (sel_score source), "selection score")
        ; ( "Authenticated"
          , (if Option.is_some (key source) then "yes" else "no")
          , "" )
        ]
      in
      let last =
        match last_sample source with
        | None -> []
        | Some s ->
            [
              ( "Last sample"
              , strf "%a" pp_ago
                  Ptime.(Span.to_float_s (diff now s.Chaos.Sample.time))
              , "" )
            ; ( "Last offset"
              , strf "%a" pp_seconds (Float.neg s.offset)
              , "positive: our clock is fast" )
            ; ( "Peer delay"
              , strf "%a" pp_delay s.peer_delay
              , "network round-trip time" )
            ; ("Peer dispersion", strf "%a" pp_delay s.peer_dispersion, "")
            ; ("Root delay", strf "%a" pp_delay s.root_delay, "")
            ; ("Root dispersion", strf "%a" pp_delay s.root_dispersion, "")
            ]
      in
      let estimations =
        if Chaos.Stats.samples stats <= 0 then []
        else
          let d = Chaos.Stats.get_tracking_data stats in
          [
            ( "Samples"
            , string_of_int (Chaos.Stats.samples stats)
            , "samples kept for the linear regression" )
          ; ( "Est. offset"
            , strf "%a ± %a" pp_seconds d.Chaos.Stats.offset pp_delay
                d.offset_sd
            , "" ); ("Est. frequency", strf "%a" pp_ppm (d.frequency *. 1e6), "")
          ; ("Est. skew", strf "%.3f ppm" (d.skew *. 1e6), "")
          ; ("Std dev", strf "%a" pp_delay (Chaos.Stats.std_dev stats), "")
          ]
      in
      let counters =
        [
          ("Sent", string_of_int counters.sent, "")
        ; ("Received", string_of_int counters.received, "")
        ; ("Timeouts", string_of_int counters.timeouts, "")
        ; ("Unreachable", string_of_int counters.unreachable, "")
        ; ("Bad packets", string_of_int counters.bad_packets, "")
        ; ( "Samples"
          , strf "%d accepted, %d rejected" counters.accepted_samples
              counters.rejected_samples
          , "" )
        ]
      in
      section buf "Source" (rows @ last);
      if estimations <> [] then section buf "Statistics" estimations;
      section buf "Counters" counters
  end;
  bpf buf
    {html|
  <hr style="border: none; border-top: 1px solid #ccc; margin-top: 3em;">
  <p><small style="color: #888;">chaos is a <a href="https://robur.coop" style="color: #888;">robur</a> project</small></p>
  <script>
    const t0 = %.3f, p0 = performance.now(), el = document.getElementById("now");
    const pad = (n, w) => String(n).padStart(w, "0");
    setInterval(() => {
      const ms = t0 + (performance.now() - p0), d = new Date(ms);
      el.textContent = d.toISOString().slice(0, 19) + "." + pad(d.getUTCMilliseconds(), 3) + "Z";
    }, 31);
  </script>
</body>
</html>
|html}
    (Ptime.to_float_s now *. 1e3);
  Buffer.contents buf

let index req _server state =
  let open Vifu.Response.Syntax in
  let* () =
    Vifu.Response.add ~field:"content-type" "text/html; charset=utf-8"
  in
  let* () = Vifu.Response.add ~field:"cache-control" "no-store" in
  let* () = Vifu.Response.with_string req (render state) in
  Vifu.Response.respond `OK

let routes =
  let open Vifu.Uri in
  let open Vifu.Route in
  [ get (rel /?? any) --> index ]

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

let run _ (cidr4, gateway4, ipv6, gateway6) cfg nameservers metrics port keys
    ckey server =
  let happy_eyeballs = happy_eyeballs cfg in
  let service =
    Mnet.stack ~name:"service" ?gateway:gateway4 cidr4 ~ipv6
      ?ipv6_gateway:gateway6
  in
  Mkernel.(run [ rng; service; metrics ])
  @@ fun _rng (stack, tcp, udp) _metrics () ->
  let@ () = fun () -> Mnet.kill stack in
  let hed, he = Mnet_happy_eyeballs.create ~happy_eyeballs tcp in
  let@ () = fun () -> Mnet_happy_eyeballs.kill hed in
  let dns = Mnet_dns.create ~nameservers (Mnet_dns.Transport.stack udp he) in
  let@ () = fun () -> Mnet_dns.Transport.kill (Mnet_dns.transport dns) in
  Tscclock.init ();
  Chaos.Clock.init Tscclock.now;
  let keys = Chaos.Auth.make keys in
  let ckey = Option.bind ckey (Chaos.Auth.find keys) in
  let daemon, ntp = Mchaos.client dns udp keys ckey server in
  let@ () = fun () -> Mchaos.stop daemon in
  let cfg = Vifu.Config.v port in
  Vifu.run ~cfg tcp routes ntp

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

let port =
  let doc = "The HTTP port" in
  let open Arg in
  value & opt int 80 & info [ "p"; "port" ] ~doc ~docv:"PORT"

let server =
  let doc =
    "The NTP server (a domain name resolved via DNS, or an IP address), with \
     an optional port."
  in
  let ( let* ) = Result.bind in
  let domain_name str =
    let* dn = Domain_name.of_string str in
    let* dn = Domain_name.host dn in
    Ok (`Domain_name dn)
  in
  let parser str =
    match Ipaddr.with_port_of_string ~default:123 str with
    | Ok (ipaddr, port) -> Ok (`Ipaddr ipaddr, port)
    | Error _ -> (
        match String.split_on_char ':' str with
        | [ host ] ->
            let* origin = domain_name host in
            Ok (origin, 123)
        | [ host; port ] -> (
            let* origin = domain_name host in
            match int_of_string_opt port with
            | Some port when port > 0 && port < 65536 -> Ok (origin, port)
            | _ -> error_msgf "Invalid port: %S" port)
        | _ -> error_msgf "Invalid NTP server: %S" str)
  in
  let pp ppf (origin, port) = Fmt.pf ppf "%a:%d" pp_origin origin port in
  let server = Arg.conv (parser, pp) in
  let open Arg in
  required & opt (some server) None & info [ "server" ] ~doc ~docv:"HOST[:PORT]"

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

let name =
  let doc = "The name of the unikernel." in
  let open Arg in
  value
  & opt string "front"
  & info [ "name" ] ~doc ~docs:docs_metrics ~docv:"NAME"

let setup_metrics ipv4 gateway dst name =
  let cfg = Option.map (fun ipv4 -> (ipv4, gateway)) ipv4 in
  let dst, port =
    match dst with
    | Some (dst, port) -> (Some dst, Some port)
    | None -> (None, None)
  in
  Tally_mnet.device ~device:"metrics" ~name cfg ?port dst

let setup_metrics =
  let open Term in
  const setup_metrics $ metrics_ipv4 $ metrics_ipv4_gateway $ metrics $ name

let keys =
  let doc =
    "A symmetric authentication key, as ID:ALGO:HEX (ALGO is SHA1 or SHA256)."
  in
  let pp ppf (k : Chaos.Auth.key) = Fmt.pf ppf "%d" k.Chaos.Auth.id in
  let key = Arg.conv (Chaos.Auth.of_cli, pp) in
  let open Arg in
  value & opt_all key [] & info [ "key" ] ~doc ~docv:"ID:ALGO:HEX"

let client_key =
  let doc =
    "Identifier of the key used to authenticate requests to upstream servers."
  in
  let open Arg in
  value & opt (some int) None & info [ "client-key" ] ~doc ~docv:"ID"

let term =
  let open Term in
  const run
  $ setup_logs
  $ Mnet_cli.setup
  $ Mnet_happy_eyeballs_cli.setup
  $ Mnet_dns_cli.setup ()
  $ setup_metrics
  $ port
  $ keys
  $ client_key
  $ server

let cmd =
  let info = Cmd.info "front" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
