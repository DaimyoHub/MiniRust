open Minimir
open Ast

let call_count = let count = ref (-1) in fun () -> incr count; !count

let rec string_of_typ = function
  | Tbool | Ti32 -> "int"
  | Tunit -> "int"
  | Tborrow (_, Mut, typ) -> (string_of_typ typ) ^ "*"
  | Tborrow (_, NotMut, typ) -> (string_of_typ typ) ^ " const*"
  | Tstruct (sname, _) -> "struct " ^ sname

let string_of_full_typ typ = function
  | Mut -> string_of_typ typ
  | NotMut -> string_of_typ typ ^ " const"

let string_of_params fdef = 
  let params = 
    List.fold_left
      (fun str (ident, mut, typ) ->
        str ^ ", " ^ string_of_full_typ typ mut ^ " " ^ ident.id)
      "" fdef.fformals
  in
  if List.length fdef.fformals > 0 then String.sub params 2 (String.length params - 2)
  else params

let string_of_local = function
  | Lparam ident -> ident
  | Lret -> "ret"
  | Lvar i -> "var" ^ string_of_int i

let rec string_of_place = function
  | PlLocal local -> string_of_local local
  | PlDeref pl -> "*" ^ string_of_place pl
  | PlField (pl, field_id) -> string_of_place pl ^ "." ^ field_id

let string_of_args args_pls = 
  let args =
    List.fold_left (fun str arg_pl -> str ^ ", " ^ string_of_place arg_pl) "" args_pls
  in
  if List.length args_pls > 0 then String.sub args 2 (String.length args - 2)
  else args

let string_of_fields fields fields_pls = 
  let fields_copy = ref (List.fold_left (fun acc (f, _) -> f.id :: acc) [] fields) in
  let args =
    List.fold_left
      (fun str arg_pl ->
        match !fields_copy with
        | field :: rem -> begin
            fields_copy := rem;
            str ^ ", ." ^ field ^ "=" ^ string_of_place arg_pl
          end
        | _ -> "" (* should be unreachable *))
      "" fields_pls
  in
  if List.length fields_pls > 0 then String.sub args 2 (String.length args - 2)
  else args

let emit_struct_assign fmt spl sdef fields_pls =
  List.iter2
    (fun pl (ident, _) ->
      Format.fprintf fmt "\t\t%s.%s = %s;\n" spl ident.id (string_of_place pl))
    fields_pls sdef.sfields

let string_of_binop = function
  | Badd -> "+"
  | Bdiv -> "/"
  | Beqeq -> "=="
  | Bge -> ">="
  | Bgt -> ">"
  | Ble -> "<="
  | Blt -> "<"
  | Bmod -> "%"
  | Bmul -> "*"
  | Bneq -> "!="
  | Bsub -> "-"

let string_of_unop = function Uneg -> "-" | Unot -> "!"

let string_of_const = function
  | Ci32 string -> String.sub string 0 (String.length string - 3)
  | Cbool b -> if b then "1" else "0"

let string_of_rvalue prog = function
  | RVbinop (op, lhs, rhs) ->
      string_of_place lhs ^ " " ^ string_of_binop op ^ " " ^ string_of_place rhs
  | RVunop (op, pl) -> string_of_unop op ^ string_of_place pl
  | RVborrow (_, pl) -> "&" ^ string_of_place pl
  | RVconst const -> string_of_const const
  | RVmake (sname, fields_pls) ->
      let sdef = get_struct_def prog sname in
      "{ " ^ string_of_fields sdef.sfields fields_pls ^ " }"
  | RVplace pl -> string_of_place pl
  | RVunit -> "0"
  
let emit_locals fmt locals ret_typ =
  Hashtbl.iter
    (fun loc typ ->
      match loc with
      | Lvar id -> Format.fprintf fmt "\t%s var%d;\n" (string_of_typ typ) id
      | Lret -> Format.fprintf fmt "\t%s ret;\n" (string_of_typ ret_typ)
      | _ -> ())
    locals

let emit_struct_fields fmt fields =
  List.iter
    (fun (ident, typ) -> Format.fprintf fmt "\t%s %s;\n" (string_of_typ typ) ident.id)
    fields

let emit_instr fmt prog label instr =
  let emit_label () = Format.fprintf fmt "\t_%d: " label in
  match instr with
  | Iassign (pl, rv, _) -> begin
      match rv with
      | RVmake (sname, fields_pls) ->
          emit_struct_assign fmt (string_of_place pl) (get_struct_def prog sname)
            fields_pls
      | _ -> ();

      emit_label ();
      Format.fprintf fmt "%s = %s;\n" (string_of_place pl) (string_of_rvalue prog rv)
    end
  | Icall (fun_name, args, ret, _) -> begin
      emit_label ();
      Format.fprintf fmt "%s = %s(%s);\n" (string_of_place ret) fun_name
        (string_of_args args)
    end
  | Igoto label -> begin
      emit_label ();
      Format.fprintf fmt "goto _%d;\n" label
    end
  | Iif (cond_pl, if_label, else_label) -> begin
      emit_label ();
      Format.fprintf fmt "if (%s) { goto _%d; } else { goto _%d; }\n"
        (string_of_place cond_pl) if_label else_label
    end
  | Ireturn -> begin
      emit_label ();
      Format.fprintf fmt "return ret;\n"
    end
  | Ideinit (_, _) -> ()

let emit_fun_body fmt prog (mir : mir_body) (fdef : fun_decl) = 
  Format.fprintf fmt "%s %s(%s) {\n" (string_of_typ fdef.freturn) fdef.fname.id
    (string_of_params fdef);

  emit_locals fmt mir.mlocals fdef.freturn;
  Format.fprintf fmt "\n";

  let instrs = Array.map (fun (instr, _) -> instr) mir.minstrs in
  Array.iteri (emit_instr fmt prog) instrs;

  Format.fprintf fmt "}\n"

let emit_struct fmt (sdef : struct_decl) =
  Format.fprintf fmt "struct %s {\n" sdef.sname.id;

  emit_struct_fields fmt sdef.sfields;

  Format.fprintf fmt "};\n\n"

let emit_fun_proto fmt fdef =
  Format.fprintf fmt "%s %s(%s);\n" (string_of_typ fdef.freturn) fdef.fname.id
    (string_of_params fdef);
  Format.fprintf fmt "\n"

let emit_struct_decl fmt sdef =
  Format.fprintf fmt "struct %s;\n" sdef.sname.id;
  Format.fprintf fmt "\n"


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

  
