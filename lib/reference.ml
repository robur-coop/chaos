[@@@warning "-26-34-36"]

let src = Logs.Src.create "chaos.reference"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
    mutable are_we_synchronised: bool
  ; mutable our_stratum: int
  ; our_ref_id: int
  ; mutable our_ref_time: Ptime.t option
  ; mutable our_skew: float
  ; mutable our_residual_freq: float
  ; mutable our_root_delay: float
  ; mutable our_root_dispersion: float
  ; mutable our_offset_sd: float
  ; mutable our_frequency_sd: float
  ; logs: Format.formatter option
}

[@@@ocamlformat "disable"]
let __line0 = "=========================================================================================================================================="
let __line1 = "   Date (UTC) Time            IP Address   St   Freq ppm   Skew ppm     Offset L Co  Offset sd Rem. corr. Root delay Root disp. Max. error"
[@@@ocamlformat "enable"]

let make ?logs () =
  let our_ref_id = Mirage_crypto_rng.generate 2 in
  let our_ref_id = String.get_int16_be our_ref_id 0 in
  let header ppf =
    Format.fprintf ppf "%s\n%!" __line0;
    Format.fprintf ppf "%s\n%!" __line1;
    Format.fprintf ppf "%s\n%!" __line0
  in
  Option.iter header logs;
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
  ; logs
  }

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

(* TODO(dinosaure): verify our offset. *)
let is_offset_ok _offset = true

let write_log =
  let last_sys_offset = ref 0.0 in
  fun t (server : Ipaddr.t * int) stratum now combined_sources freq offset
      offset_sd uncorrected_offset orig_root_distance ->
    match t.logs with
    | None -> ()
    | Some ppf ->
        let max_error = orig_root_distance +. Float.abs !last_sys_offset in
        let root_dispersion = get_root_dispersion t now in
        last_sys_offset := offset -. uncorrected_offset;
        let addr = Ipaddr.to_string (fst server) in
        let now = Fmt.str "%a" (Ptime.pp_human ()) now in
        Format.fprintf ppf
          "%s %-15s %2d %10.3f %10.3f %10.3e N %2d %10.3e %10.3e %10.3e %10.3e \
           %10.3e\n"
          now addr stratum freq (1e6 *. t.our_skew) offset combined_sources
          offset_sd uncorrected_offset t.our_root_delay root_dispersion
          max_error

let update t server ~stratum ?(combined_sources = 0) data =
  let open Stats in
  let raw = Clock.read_raw_time () in
  (* [pending] is the residual correction reported as "Rem. corr." (like
     chrony's uncorrected offset): only the frequency drift since the last
     update, not the whole cumulative software correction. [now] still uses the
     total correction so the cooked time stays exact. *)
  let pending = Clock.pending_correction raw in
  let total_corr = Clock.adjust raw in
  let total_corr = Option.get (Ptime.Span.of_float_s total_corr) in
  let now = Ptime.add_span raw total_corr in
  let now = Option.get now in
  let elapsed = Ptime.(Span.to_float_s (diff now data.ref_time)) in
  let offset = data.offset +. (elapsed *. data.frequency) in
  (* Get new estimates of the frequency and skew including the new data *)
  let freq, residual_freq, skew = clock_estimates t data in
  Log.debug (fun m ->
      m "freq=%e residual-freq=%e skew=%e" freq residual_freq skew);
  let orig_root_distance =
    (t.our_root_delay /. 2.0) +. get_root_dispersion t now
  in
  if is_offset_ok offset then begin
    t.our_stratum <- Int.min 16 (succ stratum);
    t.our_ref_time <- Some data.ref_time;
    t.our_skew <- skew;
    t.our_residual_freq <- residual_freq;
    t.our_root_delay <- data.root_delay;
    t.our_root_dispersion <- data.root_dispersion;
    t.our_frequency_sd <- data.frequency_sd;
    t.our_offset_sd <- data.offset_sd;
    let corr_rate = 0.0 in
    (* TODO(dinosaure): [corr_rate] is useless for [Clock] but it can be interesting to calculate it. *)
    Clock.accumulate_freq_and_offset ~dfreq:freq ~doffset:offset corr_rate;
    write_log t server stratum now combined_sources (Clock.frequency ()) offset
      data.offset_sd pending orig_root_distance
  end
