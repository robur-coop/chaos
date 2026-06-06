let mask = Int64.of_int 0xffffffff
let frac = (10. ** 12.) /. (2. ** 32.)

(* NOTE(dinosaure): the usage of [float] is to be more accurate
   in the conversion between NTP timestamp and [Ptime]. The fraction
   part of the NTP timestamp is nearly a multiple of ~233 if we want
   to convert it in seconds. *)

let ptime_of_int64 = function
  | 0L -> None
  | value ->
      (* NTP 32-bit seconds (since 1900) mapped to seconds since 1970 with era
         wraparound (chrony's [NTP_ERA_SPLIT = 0]): the [land 0xffffffff] makes
         the subtraction wrap in 32 bits, so the result lands in [1970, 2106) and
         timestamps past the 2036 NTP rollover decode correctly. As a bonus
         [tv_sec] is always non-negative, so the day/second split below never
         produces negative picoseconds (which would make [Ptime.v] raise). *)
      let ntp_sec =
        Int64.to_int (Int64.logand (Int64.shift_right_logical value 32) mask)
      in
      let tv_sec = (ntp_sec - 2208988800) land 0xffffffff in
      let d = tv_sec / 86400 and rem_sec = tv_sec mod 86400 in
      let tv_psec = Int64.mul (Int64.of_int rem_sec) 1_000_000_000_000L in
      let fraction = Int64.logand value mask in
      let fraction_psec = Int64.to_float fraction *. frac in
      let fraction_psec = Int64.of_float (Float.round fraction_psec) in
      let tv_psec = Int64.add tv_psec fraction_psec in
      Some (Ptime.v (d, tv_psec))

(* TODO(dinosaure): we should avoid any allocations here and in [encode_into]
   to be sure that the GC is not triggered when we craft our packet. By this
   way, we are sure that the time spent between [now ()] and the effective
   [IPv4.write] is, at least, constant. *)

let[@inline] ptime_to_int64 t =
  let span = Ptime.to_span t in
  let tv_sec = Ptime.Span.to_int_s span in
  let tv_sec = Option.value ~default:0 tv_sec in
  let tv_sec = tv_sec + 2208988800 in
  let _, tv_psec = Ptime.Span.to_d_ps span in
  (* Sub-second picoseconds -> NTP 32-bit fraction, the exact inverse of the
     decoding in [ptime_of_int64] (same rounding both ways so a timestamp
     decoded then re-encoded is bit-identical, which the strict originate-echo
     check of clients like chronyd requires). *)
  let sub_psec = Int64.rem tv_psec 1_000_000_000_000L in
  let fraction =
    Int64.of_float (Float.round (Int64.to_float sub_psec /. frac))
  in
  let fraction = Int64.min fraction mask in
  let v = Int64.(shift_left (of_int tv_sec) 32) in
  Int64.(logor v fraction)

type error =
  [ `Invalid_NTP_packet
  | `Invalid_NTP_version
  | `Server_not_in_sync
  | `Invalid_nonce ]

let pp_error ppf = function
  | `Invalid_NTP_packet -> Fmt.string ppf "Invalid NTP packet"
  | `Invalid_NTP_version -> Fmt.string ppf "Invalid NTP version"
  | `Server_not_in_sync -> Fmt.string ppf "Server not in sync"
  | `Invalid_nonce -> Fmt.string ppf "Invalid nonce"

type t = {
    flags: int
  ; stratum: int
  ; poll: int
  ; precision: int
  ; root_delay: float
  ; root_dispersion: float
  ; ref_id: int
  ; ref_ts: Ptime.t option
  ; org_ts: Ptime.t option
  ; rx_ts: Ptime.t option
  ; tx_ts: Ptime.t option
}

let pp_meta ppf t =
  Fmt.pf ppf
    "lvm=%o stratum=%d poll=%d prec=%d root_delay=%.9f root_disp=%.9f \
     ref_id=%04x"
    t.flags t.stratum t.poll t.precision t.root_delay t.root_dispersion t.ref_id

let to_sec = function
  | None -> 0.
  | Some value -> Ptime.(Span.to_float_s (to_span value))

let pp ppf t =
  Fmt.pf ppf "reference=%.9f origin=%.9f receive=%.9f transmit=%.9f"
    (to_sec t.ref_ts) (to_sec t.org_ts) (to_sec t.rx_ts) (to_sec t.tx_ts)

let ptime_of_buf buf ~off = ptime_of_int64 (String.get_int64_be buf off)

let ptime_to_buf buf ~off = function
  | None -> Bytes.set_int64_be buf off 0L
  | Some t ->
      let v = ptime_to_int64 t in
      Bytes.set_int64_be buf off v

let ptime_to_string = function
  | None -> String.make 8 '\000'
  | Some t ->
      let buf = Bytes.create 8 in
      let v = ptime_to_int64 t in
      Bytes.set_int64_be buf 0 v; Bytes.unsafe_to_string buf

let flags_to_leap v =
  match v lsr 6 with
  | 0 -> `No_warning
  | 1 -> `Minute_61
  | 2 -> `Minute_59
  | 3 -> `Unsync
  | _ -> assert false

let flags_to_version v = (v land 0x38) lsr 3

let flags_to_mode v =
  match v land 0x07 with
  | 0 -> `Reserved
  | 1 -> `Sym_a
  | 2 -> `Sym_p
  | 3 -> `Client
  | 4 -> `Server
  | 5 -> `Broadcast
  | 6 -> `Control
  | 7 -> `Private
  | _ -> assert false

let guard err fn = if fn () = false then Error err else Ok ()
let _MAX_NTP_INT32 = 4294967295. /. 65536.
let float_of_int32 r = Int32.to_float r /. 65536.

let float_to_int32 x =
  if x >= _MAX_NTP_INT32 then 0xffffffffl
  else if x <= 0. then 0l
  else
    let x = x *. 65536. in
    Int32.of_float (Float.ceil x)
(* TODO(dinosaure): verify the result! *)

let decode str =
  let ( let* ) = Result.bind in
  let* () = guard `Invalid_NTP_packet @@ fun () -> String.length str >= 48 in
  let flags = String.get_uint8 str 0 in
  let stratum = String.get_uint8 str 1 in
  let poll = String.get_uint8 str 2 in
  let precision = String.get_int8 str 3 in
  let root_delay = String.get_int32_be str 4 in
  let root_delay = float_of_int32 root_delay in
  let root_dispersion = String.get_int32_be str 8 in
  let root_dispersion = float_of_int32 root_dispersion in
  let ref_id = Int32.unsigned_to_int (String.get_int32_be str 12) in
  let ref_id = Option.get ref_id in
  let ref_ts = ptime_of_buf str ~off:16 in
  let org_ts = ptime_of_buf str ~off:24 in
  let rx_ts = ptime_of_buf str ~off:32 in
  let tx_ts = ptime_of_buf str ~off:40 in
  Ok
    {
      flags
    ; stratum
    ; poll
    ; precision
    ; root_delay
    ; root_dispersion
    ; ref_id
    ; ref_ts
    ; org_ts
    ; rx_ts
    ; tx_ts
    }

let to_string pkt =
  let buf = Bytes.create 48 in
  Bytes.set_uint8 buf 0 pkt.flags;
  Bytes.set_uint8 buf 1 pkt.stratum;
  Bytes.set_uint8 buf 2 pkt.poll;
  Bytes.set_int8 buf 3 pkt.precision;
  Bytes.set_int32_be buf 4 (float_to_int32 pkt.root_delay);
  Bytes.set_int32_be buf 8 (float_to_int32 pkt.root_dispersion);
  Bytes.set_int32_be buf 12 (Int32.of_int pkt.ref_id);
  ptime_to_buf buf ~off:16 pkt.ref_ts;
  ptime_to_buf buf ~off:24 pkt.org_ts;
  ptime_to_buf buf ~off:32 pkt.rx_ts;
  ptime_to_buf buf ~off:40 pkt.tx_ts;
  let nonce = Bytes.create 8 in
  ptime_to_buf nonce ~off:0 pkt.tx_ts;
  (Bytes.unsafe_to_string buf, Bytes.unsafe_to_string nonce)

module SBstr = Slice_bstr

let ptime_to_sbstr sbstr ~off = function
  | None -> SBstr.set_int64_be sbstr off 0L
  | Some t ->
      let v = ptime_to_int64 t in
      SBstr.set_int64_be sbstr off v

let encode_into ~now pkt bstr =
  (* TODO(dinosaure): probably use the trick about bigarrays and the layout of
     [bstr] to be sure that OCaml unboxed operations of [int64] and let it to
     use registers instead of allocating values. *)
  SBstr.set_uint8 bstr 0 pkt.flags;
  SBstr.set_uint8 bstr 1 pkt.stratum;
  SBstr.set_uint8 bstr 2 pkt.poll;
  SBstr.set_int8 bstr 3 pkt.precision;
  SBstr.set_int32_be bstr 4 (float_to_int32 pkt.root_delay);
  SBstr.set_int32_be bstr 8 (float_to_int32 pkt.root_dispersion);
  SBstr.set_int32_be bstr 12 (Int32.of_int pkt.ref_id);
  ptime_to_sbstr bstr ~off:16 pkt.ref_ts;
  ptime_to_sbstr bstr ~off:24 pkt.org_ts;
  ptime_to_sbstr bstr ~off:32 pkt.rx_ts;
  let tx_ts = now () in
  let v = ptime_to_int64 tx_ts in
  SBstr.set_int64_be bstr 40 v;
  tx_ts
