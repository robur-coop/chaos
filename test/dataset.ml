(* Parser for chrony's measurements.log and statistics.log, so a recorded chrony
   run can be replayed through chaos and compared. *)

let tokens line = String.split_on_char ' ' line |> List.filter (( <> ) "")

(* Data lines start with the date (a digit); headers start with spaces, rule
   lines with '='. *)
let is_data line = String.length line > 0 && line.[0] >= '0' && line.[0] <= '9'

let parse_time date time =
  Scanf.sscanf
    (date ^ "T" ^ time)
    "%d-%d-%dT%d:%d:%d"
    (fun y mo d hh mm ss ->
      match Ptime.of_date_time ((y, mo, d), ((hh, mm, ss), 0)) with
      | Some t -> Ptime.to_float_s t
      | None -> failwith "dataset: bad timestamp")

type measurement = {
    time: float
  ; ip: string
  ; leap: char
  ; stratum: int
  ; offset: float
  ; peer_delay: float
  ; peer_dispersion: float
  ; root_delay: float
  ; root_dispersion: float
}

let fold_lines path f acc0 =
  let ic = open_in path in
  let acc = ref acc0 in
  (try
     while true do
       let line = input_line ic in
       if is_data line then acc := f !acc (tokens line)
     done
   with End_of_file -> ());
  close_in ic; !acc

let measurements path =
  fold_lines path
    (fun acc -> function
      | date
        :: time
        :: ip
        :: l
        :: st
        :: _t1
        :: _t2
        :: _abcd
        :: _lp
        :: _rp
        :: _score
        :: off
        :: pdel
        :: pdisp
        :: rdel
        :: rdisp
        :: _ ->
          {
            time= parse_time date time
          ; ip
          ; leap= l.[0]
          ; stratum= int_of_string st
          ; offset= float_of_string off
          ; peer_delay= float_of_string pdel
          ; peer_dispersion= float_of_string pdisp
          ; root_delay= float_of_string rdel
          ; root_dispersion= float_of_string rdisp
          }
          :: acc
      | _ -> acc)
    []
  |> List.rev

type statistic = {
    time: float
  ; ip: string
  ; std_dev: float
  ; est_offset: float
  ; offset_sd: float
  ; diff_freq: float (* ppm *)
  ; est_skew: float (* ppm *)
  ; n_samples: int
  ; best_start: int
  ; n_runs: int
}

let statistics path =
  fold_lines path
    (fun acc -> function
      | date
        :: time
        :: ip
        :: sd
        :: eoff
        :: osd
        :: dfreq
        :: skew
        :: _stress
        :: ns
        :: bs
        :: nr
        :: _ ->
          {
            time= parse_time date time
          ; ip
          ; std_dev= float_of_string sd
          ; est_offset= float_of_string eoff
          ; offset_sd= float_of_string osd
          ; diff_freq= float_of_string dfreq
          ; est_skew= float_of_string skew
          ; n_samples= int_of_string ns
          ; best_start= int_of_string bs
          ; n_runs= int_of_string nr
          }
          :: acc
      | _ -> acc)
    []
  |> List.rev
