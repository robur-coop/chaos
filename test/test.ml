let () = Printexc.record_backtrace true
let passed = ref 0
let failed = ref 0

let check ?(msg = "") test =
  if test then begin
    incr passed; print_char '.'
  end
  else begin
    incr failed;
    print_char 'x';
    if msg <> "" then Fmt.pr "@.  [fail] %s" msg
  end;
  flush stdout

let check_float ?(eps = 1e-9) ?(msg = "") got expected =
  let ok = Float.abs (got -. expected) <= eps in
  let msg =
    if ok then msg
    else
      Fmt.str "%s got=%.12g expected=%.12g |Δ|=%.3g > eps=%.3g" msg got expected
        (Float.abs (got -. expected))
        eps
  in
  check ~msg ok

type t = { title: string; description: string; fn: unit -> unit }

let test ~title ~description fn = { title; description; fn }

let run tests =
  let one { title; description= _; fn } =
    Fmt.pr "%-34s " title;
    flush stdout;
    (try fn ()
     with exn ->
       incr failed;
       Fmt.pr "@.  [exn] %s@.%s" (Printexc.to_string exn)
         (Printexc.get_backtrace ()));
    Fmt.pr "@."
  in
  List.iter one tests;
  Fmt.pr "@.%d passed, %d failed@." !passed !failed;
  if !failed > 0 then exit 1
