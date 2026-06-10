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
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let make keys =
  let tbl = Hashtbl.create (max 1 (List.length keys)) in
  List.iter (fun k -> Hashtbl.replace tbl k.id k) keys;
  { keys= tbl }

let find t id = Hashtbl.find_opt t.keys id

let algo_of_string s =
  match String.uppercase_ascii s with
  | "SHA1" -> Ok SHA1
  | "SHA256" -> Ok SHA256
  | _ ->
      Error (`Msg (Fmt.str "Unknown MAC algorithm %S (use SHA1 or SHA256)" s))

let secret_of_hex h =
  let n = String.length h in
  if n = 0 || n mod 2 <> 0 then
    Error (`Msg "Key must be a non-empty even-length hex string")
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
        | None -> error_msgf "Invalid key id %S" id
      in
      let* algo = algo_of_string algo in
      let* secret = secret_of_hex hex in
      Ok { id; algo; secret }
  | _ -> error_msgf "Expected ID:ALGO:HEX"

(* Re-used scratch buffers on the hot path: the 48-byte header to hash and the
   raw digest. Sharing module-level buffers is safe because [append_into] and
   [check] contain no await point, so in Miou's cooperative single domain they
   never run concurrently nor reenter. Avoiding per-packet allocations keeps the
   minor GC out of the [t3]/[t2] timestamp window (less delay jitter). *)
let header_buf = Bytes.create 48
let digest_buf = Bytes.create 32 (* longest supported digest (SHA256) *)

(* HASH(secret || header_buf) into [digest_buf]; returns the digest length.
   Allocates only the hash context. *)
let compute_digest key =
  match key.algo with
  | SHA1 ->
      let open Digestif.SHA1 in
      get_into_bytes
        (feed_bytes (feed_string (init ()) key.secret) header_buf)
        digest_buf;
      20
  | SHA256 ->
      let open Digestif.SHA256 in
      get_into_bytes
        (feed_bytes (feed_string (init ()) key.secret) header_buf)
        digest_buf;
      32

(* Append [key_id] (offset 48) and the 20-byte (NTPv4-truncated) digest
   (offset 52) to a [Slice_bstr] whose first 48 bytes hold the NTP header. *)
let append_into key bstr =
  Slice_bstr.blit_to_bytes bstr ~src_off:0 header_buf ~dst_off:0 ~len:48;
  ignore (compute_digest key);
  Slice_bstr.set_int32_be bstr 48 (Int32.of_int key.id);
  for i = 0 to _DIGEST_LEN - 1 do
    Slice_bstr.set bstr (52 + i) (Bytes.get digest_buf i)
  done

type check = No_mac | Valid of int | Invalid

(* Constant-time comparison of [digest_buf.[0..len-1]] with [str.[off..off+len-1]]. *)
let equal_digest str ~off len =
  let acc = ref 0 in
  for i = 0 to len - 1 do
    acc :=
      !acc lor (Char.code (Bytes.get digest_buf i) lxor Char.code str.[off + i])
  done;
  !acc = 0

(* Parse the trailing MAC of a received packet and verify it over the first 48
   bytes. A peer may send the full digest (e.g. chrony in NTPv3 with SHA256 sends
   32 bytes) or a 20-byte NTPv4-truncated one, so compare over the received
   length against the full hash. *)
let check t str =
  let n = String.length str in
  if n <= 48 then No_mac
  else if n < 48 + 4 + _MIN_DIGEST_LEN then Invalid
  else
    let key_id = Int32.to_int (String.get_int32_be str 48) land 0xffffffff in
    match find t key_id with
    | None -> Invalid
    | Some key ->
        Bytes.blit_string str 0 header_buf 0 48;
        let dlen = compute_digest key in
        let m = n - 52 in
        if m >= _MIN_DIGEST_LEN && m <= dlen && equal_digest str ~off:52 m then
          Valid key_id
        else Invalid
