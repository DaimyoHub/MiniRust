open Ast

let make_struct_deps_graph prog =
  let struct_deps = Hashtbl.create 7 in
  Hashtbl.iter
    (fun _ d ->
      match d with
      | Dstruct sd ->
          Hashtbl.add struct_deps sd.sname.id
          @@ List.filter_map
               (fun (_, typ) -> match typ with Tstruct (s, _) -> Some s | _ -> None)
               sd.sfields
      | _ -> ())
    prog;
  struct_deps

let emit_proto fmt sdef =
  Format.fprintf fmt "struct %s;\n" sdef.sname.id;
  Format.fprintf fmt "\n"

let emit fmt prog =
  let struct_deps = make_struct_deps_graph prog in
  let structs_count = Hashtbl.length struct_deps in

  let already_emitted = ref [] in
  let is_in v = List.exists (fun a -> a = v) !already_emitted in
  let add v = already_emitted := v :: !already_emitted in

  let postfix source =
    let rec loop v =
      if not (is_in v) then (
        add v;
        List.iter loop (match Hashtbl.find_opt struct_deps v with Some l -> l | _ -> []);
        Emit_c.emit_struct fmt (get_struct_def prog v))
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
      (fun (sname, c) -> if c < snd !least || c = 0 then least := sname, c)
      (Seq.filter (fun (d, _) -> not (is_in d)) (Hashtbl.to_seq deps_counts));

    if fst !least <> "" then Some (fst !least) else None
  in

  while List.length !already_emitted < structs_count do
    match get_least_dep () with Some d -> postfix d | _ -> ()
  done
