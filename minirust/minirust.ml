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


    if Sys.getenv_opt "C" <> None && Sys.getenv_opt "MINIMIR" = None then begin

      (* Struct deps graph handling : BEGIN ********************************************)

      let struct_deps = Emit_c.make_struct_deps_graph prog in
      let structs_count = Hashtbl.length struct_deps in

      let already_emitted = ref [] in
      let is_in v = List.exists (fun a -> a = v) !already_emitted in 
      let add v = already_emitted := v :: !already_emitted in

      let postfix source =
        let rec loop v =
          if not (is_in v) then  begin
            add v;
            List.iter loop
              (match Hashtbl.find_opt struct_deps v with Some l -> l | _ -> []);
            Emit_c.emit_struct Format.std_formatter (get_struct_def prog v)
          end
        in
        loop source
      in

      let get_least_dep () =
        let deps_counts = Hashtbl.create 4 in
        Hashtbl.iter 
          (fun d deps ->
            if Hashtbl.find_opt deps_counts d = None then Hashtbl.add deps_counts d 0;
            List.iter
              (fun dep ->
                Hashtbl.replace deps_counts dep
                  (match Hashtbl.find_opt deps_counts dep with Some c -> c + 1 | _ -> 1))
              deps)
          struct_deps;

        let least = ref ("", 1000) in
        Seq.iter
          (fun (sname, c) -> if c < (snd !least) || c = 0 then least := sname, c)
          (Seq.filter (fun (d, _) -> not (is_in d)) (Hashtbl.to_seq deps_counts));
 
        if fst !least <> "" then Some (fst !least) else None
      in

      while List.length !already_emitted < structs_count do
        match get_least_dep () with
        | Some d -> postfix d
        | _ -> ()
      done;

      (* Struct deps graph handling : END **********************************************)

      Hashtbl.iter
        (fun _ d ->
            match d with
            | Dfundef fd -> Emit_c.emit_fun_proto Format.std_formatter fd
            | _ -> ())
        prog;

      Hashtbl.iter
        (fun _ d ->
            match d with
            | Dfundef fd -> begin
                let mir = Hashtbl.find fdefs fd in
                Emit_c.emit_fun_body Format.std_formatter prog mir fd;
                Printf.printf "\n"
              end
            | _ -> ())
        prog
    end

  with Error.Error (start, end_, msg) ->
    Printf.eprintf "%s" (MenhirLib.LexerUtil.range (start, end_));
    Printf.eprintf "%s\n" msg;
    exit 1
