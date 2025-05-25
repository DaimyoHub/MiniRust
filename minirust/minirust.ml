open Minirust
open Ast

let () =
  if Array.length Sys.argv != 2 then Error.usage ();
  let filename = Sys.argv.(1) in
  try
    let prog = Parser_entry.parse_file filename in

    Typecheck.go prog;

    let fdefs = Hashtbl.create 7 in

    Hashtbl.iter
      (fun _ d ->
        match d with
        | Dfundef fd ->
            let mir = Emit_minimir.emit_fun prog fd in
            Hashtbl.add fdefs fd mir;

            if Sys.getenv_opt "MINIMIR" <> None then (
              Printf.printf "=== %s ===\n" fd.fname.id;
              Print_minimir.print mir;
              Printf.printf "\n");

            Borrowck.borrowck prog mir
        | _ -> ())
      prog;

    if Sys.getenv_opt "C" <> None && Sys.getenv_opt "MINIMIR" = None then (
      Emit_c_structs.emit prog;

      Hashtbl.iter
        (fun _ d ->
          match d with
          | Dfundef fd -> Emit_c.emit_fun_proto Format.std_formatter fd
          | _ -> ())
        prog;

      Hashtbl.iter
        (fun _ d ->
          match d with
          | Dfundef fd ->
              let mir = Hashtbl.find fdefs fd in
              Emit_c.emit_fun_body Format.std_formatter prog mir fd;
              Printf.printf "\n"
          | _ -> ())
        prog)
  with Error.Error (start, end_, msg) ->
    Printf.eprintf "%s" (MenhirLib.LexerUtil.range (start, end_));
    Printf.eprintf "%s\n" msg;
    exit 1
