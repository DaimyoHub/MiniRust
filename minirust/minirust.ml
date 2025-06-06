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

            if Sys.getenv_opt "MINIMIR" <> None && Sys.getenv_opt "C" = None then (
              Printf.printf "=== %s ===\n" fd.fname.id;
              Print_minimir.print mir;
              Printf.printf "\n");

            Borrowck.borrowck prog mir
        | _ -> ())
      prog;

    if Sys.getenv_opt "C" <> None && Sys.getenv_opt "MINIMIR" = None then (
      let basename =
        match String.split_on_char '.' filename with
        | x :: _ -> x
        | [] -> "" (* should be unreachable ? *)
      in
      let output_file = open_out (basename ^ ".c") in
      let fmt = Format.formatter_of_out_channel output_file in

      Hashtbl.iter
        (fun _ d ->
          match d with
          | Dstruct sd -> Emit_c_structs.emit_proto fmt sd
          | _ -> ())
        prog;

      Emit_c_structs.emit fmt prog;

      Hashtbl.iter
        (fun _ d ->
          match d with
          | Dfundef fd -> Emit_c.emit_fun_proto fmt fd
          | _ -> ())
        prog;

      Hashtbl.iter
        (fun _ d ->
          match d with
          | Dfundef fd ->
              let mir = Hashtbl.find fdefs fd in
              Emit_c.emit_fun_body fmt prog mir fd;
              Printf.printf "\n"
          | _ -> ())
        prog;

      close_out output_file);
  with Error.Error (start, end_, msg) ->
    Printf.eprintf "%s" (MenhirLib.LexerUtil.range (start, end_));
    Printf.eprintf "%s\n" msg;
    exit 1
