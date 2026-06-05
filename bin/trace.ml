let run filename destination =
  let ic = open_in_bin filename in
  let oc = open_out_bin destination in
  let finally () =
    close_in ic;
    close_out oc in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  really_input ic buf 0 8;
  let real_length = Bytes.get_int64_be buf 0 in
  if in_channel_length ic < Int64.to_int real_length
  then `Error (false, "truncated memtrace file")
  else
    let rec go = function
      | 0L -> ()
      | remaining ->
          let llen = Int64.min 0x7ffL remaining in
          let len = Int64.to_int llen in
          really_input ic buf 0 len;
          output_substring oc (Bytes.unsafe_to_string buf) 0 len;
          go (Int64.sub remaining llen) in
    go (Int64.sub real_length 8L); `Ok ()

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Cmdliner

let image =
  let doc = "The file used by the unikernel to keep the memory trace." in
  let parser str =
    if Sys.file_exists str
    && Sys.is_directory str = false
    then Ok str
    else error_msgf "%S does not exist" str in
  let open Arg in
  required & pos 0 (some (conv (parser, Fmt.string))) None & info [] ~doc ~docv:"FILENAME"

let destination =
  let doc = "The destination of the memory trace." in
  let open Arg in
  required & pos 1 (some string) None & info [] ~doc ~docv:"FILENAME"

let term =
  let open Term in
  ret (const run $ image $ destination)

let cmd =
  let doc = "A tool to transform memory trace from any unikernel to a memtrace file (which can be visualised with $(b,memtrace-viewer))." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) is a simple program to transform what is recorded by an unikernel to a memtrace file which can be visualised by \
          $(b,memtrace-viewer)." ] in
  let info = Cmd.info "mtrace" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
