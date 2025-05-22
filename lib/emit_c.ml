(*
open Minimir
open Ast

type offsets = (local, int) Hashtbl.t

let call_count = let count = ref (-1) in fun () -> incr count; !count

let rec string_of_typ = function
  | Tbool | Ti32 -> "int"
  | Tunit -> "void"
  | Tborrow (_, Mut, typ) -> (string_of_typ typ) ^ "*"
  | Tborrow (_, NotMut, typ) -> (string_of_typ typ) ^ " const*"
  | Tstruct (sname, _) -> "struct " ^ sname

let emit_params fmt fun_name locals = 
  Format.fprintf fmt "struct _%s_args {\n" fun_name;
  Hashtbl.iter
    (fun loc typ ->
      match loc with
      | Lparam pname -> Format.fprintf fmt "%s %s;\n" (string_of_typ typ) pname
      | _ -> ())
    locals;
  Format.fprintf fmt "};\n\n"

let emit_locals fmt locals ret_typ =
  Hashtbl.iter
    (fun loc typ ->
      match loc with
      | Lvar id -> Format.fprintf fmt "%s var%d;\n" (string_of_typ typ) id
      | Lret -> Format.fprintf fmt "%s ret;\n" (string_of_typ ret_typ)
      | _ -> ())
    locals

let emit_args fmt label fun_name args =
  Format.fprintf fmt "struct _%s_args args%d;\n" fun_name label;
  List.iter
    (fun arg -> Format.fprintf fmt "args%d.")

let emit_instr fmt label instr =
  Format.fprintf fmt "_%s: " label;
  match instr with
  | Iassign (pl, rv, _) ->
      Format.fprintf fmt "%s = %s;\n" (string_of_place pl) (string_of_rvalue rv)
  | Icall (fun_name, args, ret, _) ->
      Format.fprintf fmt "%s = %s(%s);\n"
        (string_of_place ret) fun_name (string_of_args args)

let emit_fun fmt prog (mir : mir_body) fun_name = 
  let fun_def = get_fun_def prog fun_name in

  emit_params fmt fun_name mir.mlocals;

  Format.fprintf fmt "%s %s(struct _%s_args params) {\n"
    (string_of_typ fun_def.freturn)
    fun_name fun_name;

  (* Then we add the locals *)
  emit_locals fmt mir.mlocals fun_def.freturn;

  let instrs = Array.map (fun (instr, _) -> instr) mir.minstrs in
  Array.iteri (emit_instr fmt) instrs
*)
