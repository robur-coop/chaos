let mask = Int64.of_int 0xffffffff
let frac = (10. ** 12.) /. (2. ** 32.)

(* NOTE(dinosaure): the usage of [float] is to be more accurate
   in the conversion between NTP timestamp and [Ptime]. The fraction
   part of the NTP timestamp is nearly a multiple of ~233 if we want
   to convert it in seconds. *)

let ptime_of_int64 = function
  | 0L -> None
  | value ->
      let tv_sec = Int64.(logand (shift_right value 32) mask) in
      let tv_sec = Int64.sub tv_sec 2208988800L in
      (* 1 Jan 1900 to 1 Jan 1970 *)
      let rem_sec = Int64.rem tv_sec 86400L in
      let d = Int64.to_int (Int64.div tv_sec 86400L) in
      let tv_psec = Int64.mul rem_sec 1_000_000_000_000L in
      let fraction = Int64.logand value mask in
      let fraction_psec = Int64.to_float fraction *. frac in
      let fraction_psec = Int64.of_float fraction_psec in
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
  let tv_psec = Int64.to_float tv_psec in
  let frac_s = tv_psec /. 1e12 in
  let frac_s = Int64.of_float frac_s in
  let v = Int64.(shift_left (of_int tv_sec) 32) in
  Int64.(logor v (logand frac_s mask))

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
  ; refid: int32
  ; ref_ts: Ptime.t option
  ; org_ts: Ptime.t option
  ; rx_ts: Ptime.t option
  ; tx_ts: Ptime.t option
}

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

let pp ppf pkt =
  let pp ppf = function
    | None -> Fmt.string ppf "NULL"
    | Some v -> Ptime.pp ppf v
  in
  Fmt.pf ppf
    "{ @[<hov>flags= %02x;@ stratum= %d;@ poll= %02x;@ precision= %02x;@ \
     root_delay= %f;@ root_dispersion= %f;@ refid= %ld;@ reference_ts= %a;@ \
     origin_ts= %a;@ recv_ts= %a;@ trans_ts= %a;@] }"
    pkt.flags pkt.stratum pkt.poll pkt.precision pkt.root_delay
    pkt.root_dispersion pkt.refid pp pkt.ref_ts pp pkt.org_ts pp pkt.rx_ts pp
    pkt.tx_ts

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

let decode ?nonce str =
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
  let refid = String.get_int32_be str 12 in
  let ref_ts = ptime_of_buf str ~off:16 in
  let org_ts = ptime_of_buf str ~off:24 in
  let rx_ts = ptime_of_buf str ~off:32 in
  let tx_ts = ptime_of_buf str ~off:40 in
  let* () =
    guard `Invalid_NTP_version @@ fun () -> flags_to_version flags == 4
  in
  let* () =
    guard `Server_not_in_sync @@ fun () ->
    Option.is_some rx_ts && Option.is_some tx_ts
  in
  let* () =
    guard `Invalid_nonce @@ fun () ->
    match nonce with
    | None -> true
    | Some nonce -> String.equal nonce (String.sub str 24 8)
  in
  Ok
    {
      flags
    ; stratum
    ; poll
    ; precision
    ; root_delay
    ; root_dispersion
    ; refid
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
  Bytes.set_uint8 buf 3 pkt.precision;
  Bytes.set_int32_be buf 4 (float_to_int32 pkt.root_delay);
  Bytes.set_int32_be buf 8 (float_to_int32 pkt.root_dispersion);
  Bytes.set_int32_be buf 12 pkt.refid;
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
  SBstr.set_uint8 bstr 3 pkt.precision;
  SBstr.set_int32_be bstr 4 (float_to_int32 pkt.root_delay);
  SBstr.set_int32_be bstr 8 (float_to_int32 pkt.root_dispersion);
  SBstr.set_int32_be bstr 12 pkt.refid;
  ptime_to_sbstr bstr ~off:16 pkt.ref_ts;
  ptime_to_sbstr bstr ~off:24 pkt.org_ts;
  ptime_to_sbstr bstr ~off:32 pkt.rx_ts;
  let tx_ts = now () in
  let v = ptime_to_int64 tx_ts in
  SBstr.set_int64_be bstr 40 v;
  tx_ts
