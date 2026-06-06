(* NTP symmetric-key authentication (RFC 5905 "NTP MAC"), as chrony does in the
   [NTP_AUTH_SYMMETRIC] mode. After the 48-byte NTP header a MAC is appended:
   a 4-byte big-endian [key_id] followed by a digest. The digest is the keyed
   hash [HASH(secret || header)] (chrony's [HSH_Hash], not HMAC), truncated to
   at most 20 bytes for NTPv4 (RFC 7822). *)

let src = Logs.Src.create "chaos.auth"

module Log = (val Logs.src_log src : Logs.LOG)

type algo = SHA1 | SHA256
type key = { id: int; algo: algo; secret: string }
type t = { keys: (int, key) Hashtbl.t }

let _DIGEST_LEN = 20 (* NTP_MAX_V4_MAC_LENGTH - 4 *)
let _MIN_DIGEST_LEN = 16 (* NTP_MIN_MAC_LENGTH - 4 *)
let mac_length = 4 + _DIGEST_LEN

let make keys =
  let tbl = Hashtbl.create (max 1 (List.length keys)) in
  List.iter (fun k -> Hashtbl.replace tbl k.id k) keys;
  { keys= tbl }

let find t id = Hashtbl.find_opt t.keys id

let algo_of_string s =
  match String.uppercase_ascii s with
  | "SHA1" -> Ok SHA1
  | "SHA256" -> Ok SHA256
  | _ -> Error (`Msg (Fmt.str "Unknown MAC algorithm %S (use SHA1 or SHA256)" s))

let secret_of_hex h =
  let n = String.length h in
  if n = 0 || n mod 2 <> 0 then Error (`Msg "Key must be a non-empty even-length hex string")
  else
    try
      let b = Bytes.create (n / 2) in
      for i = 0 to (n / 2) - 1 do
        Bytes.set_uint8 b i (int_of_string ("0x" ^ String.sub h (2 * i) 2))
      done;
      Ok (Bytes.unsafe_to_string b)
    with _ -> Error (`Msg "Key contains invalid hex characters")

(* Parse a CLI key specification "ID:ALGO:HEX". *)
let of_cli spec =
  let ( let* ) = Result.bind in
  match String.split_on_char ':' spec with
  | [ id; algo; hex ] ->
      let* id =
        match int_of_string_opt id with
        | Some id -> Ok id
        | None -> Error (`Msg (Fmt.str "Invalid key id %S" id))
      in
      let* algo = algo_of_string algo in
      let* secret = secret_of_hex hex in
      Ok { id; algo; secret }
  | _ -> Error (`Msg "Expected ID:ALGO:HEX")

(* Full keyed hash HASH(secret || msg) (20 bytes for SHA1, 32 for SHA256). *)
let digest_full key msg =
  match key.algo with
  | SHA1 ->
      Digestif.SHA1.(
        to_raw_string (digesti_string (fun f -> f key.secret; f msg)))
  | SHA256 ->
      Digestif.SHA256.(
        to_raw_string (digesti_string (fun f -> f key.secret; f msg)))

(* What we put on the wire: the digest truncated to 20 bytes (NTPv4, RFC 7822). *)
let digest key msg =
  let raw = digest_full key msg in
  if String.length raw <= _DIGEST_LEN then raw else String.sub raw 0 _DIGEST_LEN

(* Constant-time comparison over the common length (length is not secret). *)
let equal_ct a b =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri
    (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i]))
    a;
  !acc = 0

(* Append [key_id] (offset 48) and the digest (offset 52) to a [Slice_bstr]
   whose first 48 bytes already hold the NTP header. *)
let append_into key bstr =
  let header = String.init 48 (fun i -> Slice_bstr.get bstr i) in
  let d = digest key header in
  Slice_bstr.set_int32_be bstr 48 (Int32.of_int key.id);
  String.iteri (fun i c -> Slice_bstr.set bstr (52 + i) c) d

type check = No_mac | Valid of int | Invalid

(* Parse the trailing MAC of a received packet and verify it over the first 48
   bytes. *)
let check t str =
  let n = String.length str in
  if n <= 48 then No_mac
  else if n < 48 + 4 + _MIN_DIGEST_LEN then Invalid
  else
    let key_id = Int32.to_int (String.get_int32_be str 48) land 0xffffffff in
    let recv = String.sub str 52 (n - 52) in
    match find t key_id with
    | None -> Invalid
    | Some key ->
        (* Compare against the FULL hash truncated to the received length: a peer
           may send the full digest (e.g. chrony in NTPv3 with SHA256 sends 32
           bytes) or a 20-byte NTPv4-truncated one. *)
        let expected = digest_full key (String.sub str 0 48) in
        let m = String.length recv in
        if
          m >= _MIN_DIGEST_LEN
          && m <= String.length expected
          && equal_ct (String.sub expected 0 m) recv
        then Valid key_id
        else Invalid
