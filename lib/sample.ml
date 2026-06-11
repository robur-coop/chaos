(* Structure to save NTP measurements.

   - [time] is the local time at which the sample is to be considered to have
     been made
   - and [offset] is the offset at the time (positive indicates that the local
     clock is slow relative to the source).
   - [root_delay]/[root_dispersion] include [peer_delay]/[peer_dispersion]
*)

type t = {
    time: Ptime.t
  ; offset: float
  ; peer_delay: float
  ; peer_dispersion: float
  ; root_delay: float
  ; root_dispersion: float
}

let ps_per_ns = 1_000L
let ps_per_s = 1_000_000_000_000L

let make ~offset ?(dispersion = 1e-6) ~delay secs =
  let time = Ptime.of_float_s secs in
  let time = Option.get time in
  {
    time
  ; offset
  ; peer_delay= delay
  ; peer_dispersion= dispersion
  ; root_delay= delay
  ; root_dispersion= dispersion
  }

let to_timespec t =
  let d, ps = Ptime.(Span.to_d_ps (to_span t)) in
  let tv_sec = 86400 * d in
  let tv_sec = tv_sec + Int64.(to_int (div ps ps_per_s)) in
  let rem_psec = Int64.(rem ps ps_per_s) in
  let tv_nsec = Int64.(div rem_psec ps_per_ns) in
  (tv_sec, tv_nsec)

let pp_like_c ppf t =
  let tv_sec, tv_nsec = to_timespec t.time in
  Fmt.pf ppf
    "{ .time= { .tv_sec= %d, .tv_nsec= %Ld }, .offset= %e, .peer_delay= %e, \
     .peer_dispersion= %e, .root_delay= %e, .root_dispersion= %e }"
    tv_sec tv_nsec t.offset t.peer_delay t.peer_dispersion t.root_delay
    t.root_dispersion

let pp_like_ocaml ppf t =
  let d, ps = Ptime.(Span.to_d_ps (to_span t.time)) in
  Fmt.pf ppf
    "{ time= Ptime.unsafe_of_d_ps (%d, %LdL); offset= %e; peer_delay= %e; \
     peer_dispersion= %e; root_delay= %e; root_dispersion= %e }"
    d ps t.offset t.peer_delay t.peer_dispersion t.root_delay t.root_dispersion
