(** NTP symmetric-key authentication (RFC 5905 "NTP MAC"): a [key_id] + keyed
    hash digest appended after the 48-byte NTP header. *)

type algo = SHA1 | SHA256
type key = { id: int; algo: algo; secret: string }

type t
(** A keystore mapping key ids to keys. *)

val make : key list -> t
val find : t -> int -> key option

val of_cli : string -> (key, [> `Msg of string ]) result
(** [of_cli "ID:ALGO:HEX"] parses a key specification given on the command line.
    [ALGO] is [SHA1] or [SHA256], [HEX] the hex-encoded secret. *)

val mac_length : int
(** Length in bytes of the MAC appended to an authenticated packet (4 + 20). *)

val append_into : key -> Slice_bstr.t -> unit
(** [append_into key bstr] writes the [key_id] (offset 48) and the digest of the
    first 48 bytes (offset 52) into [bstr], which must be at least
    [48 + mac_length] bytes long. *)

type check =
  | No_mac  (** the packet carries no MAC *)
  | Valid of int  (** valid MAC; the key id used *)
  | Invalid  (** a MAC is present but unknown key or wrong digest *)

val check : t -> string -> check
(** [check t raw] parses the trailing MAC of a received packet [raw] and verifies
    it against the keystore over the first 48 bytes. *)
