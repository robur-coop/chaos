let src = Logs.Src.create "chaos.reference"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
    mutable are_we_synchronised: bool
  ; mutable our_stratum: int
  ; mutable our_ref_id: int
  ; mutable our_ref_time: Ptime.t option
  ; mutable our_skew: float
  ; mutable our_residual_freq: float
  ; mutable our_root_delay: float
  ; mutable our_root_dispersion: float
  ; mutable our_offset_sd: float
  ; mutable our_frequency_sd: float
  ; mutable our_leap_status: int
        (* Leap indicator (NTP encoding): 0 normal, 1 insert, 2 delete, 3
         unsynchronised. *)
  ; max_offset: float
        (* chrony's [maxchange]: maximum allowed clock correction. *)
  ; mutable max_offset_delay: int
        (* Number of initial updates to accept unconditionally; [< 0] disables
         the check entirely (chrony default). *)
  ; mutable max_offset_ignore: int
        (* Number of remaining over-the-maximum corrections to tolerate. *)
  ; logs: Format.formatter option
  ; mutable last_offset: float
  ; mutable avg2_offset: float
  ; mutable avg2_moving: bool
  ; mutable combined_sources: int
  ; mutable updates: int
  ; mutable rejected_updates: int
}

(* Indexed by the NTP leap indicator, like chrony's [leap_codes]. *)
let leap_codes = [| 'N'; '+'; '-'; '?' |]

(* NTP reference id of the source we synchronise to. For a stratum >= 2 server
   this is, by convention, the IPv4 address of the reference source. For IPv6 we
   fall back to the low 32 bits of a hash of the address (our upstreams are IPv4
   in practice). *)
let refid_of_ipaddr = function
  | Ipaddr.V4 v4 -> Int32.to_int (Ipaddr.V4.to_int32 v4) land 0xffffffff
  | Ipaddr.V6 v6 ->
      let s = Ipaddr.V6.to_octets v6 in
      String.get_int32_be s 0 |> Int32.to_int |> ( land ) 0xffffffff

(* [max_change] mirrors chrony's [maxchange offset delay ignore] directive:
   reject (and skip) a clock correction larger than [offset] once the first
   [delay] updates have passed, tolerating [ignore] further violations. The
   default [(0., -1, 0)] disables the check, like chrony. *)
let make ?logs ?(max_change = (0.0, -1, 0)) () =
  let max_offset, max_offset_delay, max_offset_ignore = max_change in
  let our_ref_id = Mirage_crypto_rng.generate 2 in
  let our_ref_id = String.get_int16_be our_ref_id 0 in
  {
    are_we_synchronised= false
  ; our_root_dispersion= 1.0
  ; our_root_delay= 1.0
  ; our_skew=
      1.0 (* NOTE(dinosaure): really bad skew, we should be less than 1e-3. *)
  ; our_frequency_sd= 0.0
  ; our_residual_freq= 0.0
  ; our_offset_sd= 0.0
  ; our_stratum= 0
  ; our_ref_id
  ; our_ref_time= None
  ; our_leap_status= 3 (* LEAP_Unsynchronised until the first update *)
  ; max_offset
  ; max_offset_delay
  ; max_offset_ignore
  ; logs
  ; last_offset= 0.0
  ; avg2_offset= 0.0
  ; avg2_moving= false
  ; combined_sources= 0
  ; updates= 0
  ; rejected_updates= 0
  }

let update_rms_offset t offset =
  let offset2 = offset *. offset in
  if t.avg2_moving then
    t.avg2_offset <- t.avg2_offset +. (0.1 *. (offset2 -. t.avg2_offset))
  else begin
    if t.avg2_offset > 0.0 && t.avg2_offset < offset2 then t.avg2_moving <- true;
    t.avg2_offset <- offset2
  end

let square x = x *. x
let clamp ~min:mi ~max:ma value = Float.max (Float.min value ma) mi

let clock_estimates t data =
  let open Stats in
  let measured_freq = data.frequency in
  let measured_skew = data.skew in
  if Float.abs measured_skew > 1e-3 then
    Log.warn (fun m -> m "skew %f too large to track" measured_skew);
  (* Set new frequency based on weigthed average of the expected and measured
     skew. Disable updates that are based on totally unreliable frequency
     information. *)
  let gain =
    if Float.abs measured_skew > 1e-3 then 0.0
    else
      3.0
      *. square t.our_skew
      /. ((3.0 *. square t.our_skew) +. square measured_skew)
  in
  let gain = clamp ~min:0. ~max:1. gain in
  let estimated_freq = gain *. measured_freq in
  let residual_freq = measured_freq -. estimated_freq in
  let extra_skew =
    Float.sqrt
      ((square (Float.neg estimated_freq) *. (1.0 -. gain))
      +. (square (measured_freq -. estimated_freq) *. gain))
  in
  let estimated_skew =
    t.our_skew +. (gain *. (measured_skew -. t.our_skew)) +. extra_skew
  in
  (estimated_freq, residual_freq, estimated_skew)

let get_root_dispersion t now =
  match t.our_ref_time with
  | Some our_ref_time ->
      let diff = Ptime.(Span.to_float_s (diff now our_ref_time)) in
      t.our_root_dispersion
      +. Float.abs diff
         *. (t.our_skew +. Float.abs t.our_residual_freq +. 1e-6)
  | None -> 1.0

(* chrony's [is_offset_ok] (the [maxchange] feature): once the initial [delay]
   updates have passed, reject a correction larger than [max_offset]. Unlike
   chrony, which exits the daemon when [max_offset_ignore] reaches zero, we only
   skip the offending update (returning [false]) so the unikernel keeps running.
   Disabled by default ([max_offset_delay < 0]). *)
let is_offset_ok t offset =
  if t.max_offset_delay < 0 then true
  else if t.max_offset_delay > 0 then begin
    t.max_offset_delay <- t.max_offset_delay - 1;
    true
  end
  else if Float.abs offset > t.max_offset then begin
    Log.warn (fun m ->
        m
          "Adjustment of %.3f seconds exceeds the allowed maximum of %.3f \
           seconds (ignored)"
          (Float.neg offset) t.max_offset);
    if t.max_offset_ignore > 0 then
      t.max_offset_ignore <- t.max_offset_ignore - 1;
    false
  end
  else true

let update t server ~stratum ?(combined_sources = 0) ?(leap = 0) data =
  let open Stats in
  let raw = Clock.read_raw_time () in
  (* [pending] is the residual correction reported as "Rem. corr." (like
     chrony's uncorrected offset): only the frequency drift since the last
     update, not the whole cumulative software correction. [now] still uses the
     total correction so the cooked time stays exact. *)
  let _pending = Clock.pending_correction raw in
  let now = Clock.cook raw in
  let elapsed = Ptime.(Span.to_float_s (diff now data.ref_time)) in
  let offset = data.offset +. (elapsed *. data.frequency) in
  (* Get new estimates of the frequency and skew including the new data *)
  let freq, residual_freq, skew = clock_estimates t data in
  Log.debug (fun m ->
      m "freq=%e residual-freq=%e skew=%e" freq residual_freq skew);
  let _orig_root_distance =
    (t.our_root_delay /. 2.0) +. get_root_dispersion t now
  in
  if is_offset_ok t offset then begin
    t.are_we_synchronised <- true;
    t.our_leap_status <- leap;
    t.our_ref_id <- refid_of_ipaddr (fst server);
    t.our_stratum <- Int.min 16 (succ stratum);
    t.our_ref_time <- Some data.ref_time;
    t.our_skew <- skew;
    t.our_residual_freq <- residual_freq;
    t.our_root_delay <- data.root_delay;
    t.our_root_dispersion <- data.root_dispersion;
    t.our_frequency_sd <- data.frequency_sd;
    t.our_offset_sd <- data.offset_sd;
    t.last_offset <- offset;
    t.combined_sources <- combined_sources;
    t.updates <- t.updates + 1;
    update_rms_offset t offset;
    Clock.accumulate_freq_and_offset ~dfreq:freq ~doffset:offset
  end
  else t.rejected_updates <- t.rejected_updates + 1

(* Reference parameters exposed to the NTP server side, mirroring chrony's
   [REF_GetReferenceParams] (without the local-stratum fallback). *)
type params = {
    synchronised: bool
  ; leap: int
  ; stratum: int
  ; ref_id: int
  ; ref_time: Ptime.t
  ; root_delay: float
  ; root_dispersion: float
}

let get_params t now =
  match (t.are_we_synchronised, t.our_ref_time) with
  | true, Some ref_time ->
      {
        synchronised= true
      ; leap= t.our_leap_status
      ; stratum= t.our_stratum
      ; ref_id= t.our_ref_id
      ; ref_time
      ; root_delay= t.our_root_delay
      ; root_dispersion= get_root_dispersion t now
      }
  | _ ->
      {
        synchronised= false
      ; leap= 3 (* LEAP_Unsynchronised *)
      ; stratum= 0
      ; ref_id= 0
      ; ref_time= Ptime.epoch
      ; root_delay= 0.0
      ; root_dispersion= 0.0
      }

type tracking = {
    synchronised: bool
  ; leap: int
  ; stratum: int
  ; ref_id: int
  ; last_ref_time: Ptime.t option
  ; last_offset: float
  ; rms_offset: float
  ; skew: float
  ; residual_freq: float
  ; root_delay: float
  ; root_dispersion: float
  ; offset_sd: float
  ; frequency_sd: float
  ; combined_sources: int
  ; updates: int
  ; rejected_updates: int
}

let tracking t now =
  {
    synchronised= t.are_we_synchronised
  ; leap= t.our_leap_status
  ; stratum= t.our_stratum
  ; ref_id= t.our_ref_id
  ; last_ref_time= t.our_ref_time
  ; last_offset= t.last_offset
  ; rms_offset= Float.sqrt t.avg2_offset
  ; skew= t.our_skew
  ; residual_freq= t.our_residual_freq
  ; root_delay= t.our_root_delay
  ; root_dispersion= get_root_dispersion t now
  ; offset_sd= t.our_offset_sd
  ; frequency_sd= t.our_frequency_sd
  ; combined_sources= t.combined_sources
  ; updates= t.updates
  ; rejected_updates= t.rejected_updates
  }
