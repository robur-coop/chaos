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
