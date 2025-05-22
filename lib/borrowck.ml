(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-26-27"]

open Type
open Minimir
open Active_borrows

(* This function computes the set of alive lifetimes at every program point. *)
let compute_lft_sets prog mir : lifetime -> PpSet.t =
  (* The [outlives] variable contains all the outlives relations between the
    lifetime variables of the function. *)
  let outlives = ref LMap.empty in

  (* Helper functions to add outlives constraints. *)
  let add_outlives (l1, l2) = outlives := add_outlives_edge l1 l2 !outlives in
  let unify_lft l1 l2 =
    add_outlives (l1, l2);
    add_outlives (l2, l1)
  in

  (* First, we add in [outlives] the constraints implied by the type of locals. *)
  Hashtbl.iter
    (fun _ typ -> outlives := outlives_union !outlives (implied_outlives prog typ))
    mir.mlocals;

  (* Then, we add the outlives relations needed for the instructions to be safe. *)
  let instrs_count = Array.length mir.minstrs in

  (* Task n°3 : BEGIN ******************************************************************)

  (*: generate these constraints by
       - unifying types that need be equal (note that MiniRust does not support subtyping,
         that is, if a variable x: &'a i32 is used as type &'b i32, then this requires
         that lifetimes 'a and 'b are equal),
       - adding constraints required by function calls,
       - generating constraints corresponding to reborrows. More precisely, if we create
         a borrow of a place that dereferences  borrows, then the lifetime of the borrow
         we create should be shorter than the lifetimes of the borrows the place
         dereference. For example, if x: &'a &'b i32, and we create a borrow y = &**x of
         type &'c i32, then 'c should be shorter than 'a and 'b.

    SUGGESTION: use functions [typ_of_place], [fields_types_fresh] and [fn_prototype_fresh].
  *)
  let rec unify_typs_lfts typ1 typ2 =
    match typ1, typ2 with
    | Tstruct (_, lfts1), Tstruct (_, lfts2) ->
        List.iter2 (fun lft1 lft2 -> unify_lft lft1 lft2) lfts1 lfts2
    | Tborrow (lft1, _, utyp1), Tborrow (lft2, _, utyp2) ->
        unify_lft lft1 lft2;
        unify_typs_lfts utyp1 utyp2
    (* Cases where one type is different from another should not be reached since the 
       typechecker has already checks the program at this point. *)
    | _ -> ()
  in

  let get_lft = function Tborrow (lft, _, _) -> Some lft | _ -> None in

  let top = typ_of_place prog mir in
  Array.iteri
    (fun label (instr, _) ->
      match instr with
      | Ideinit (_, _) | Iif (_, _, _) | Igoto _ | Ireturn -> ()
      | Iassign (pl, rv, _) -> (
          let pl_typ = top pl in
          match rv with
          (* If the rvalue is place or an operation between places, it is straightforward,
             we just unify lvalue's and rvalue's lifetimes. *)
          | RVplace rv_pl | RVunop (_, rv_pl) ->
              let rv_pl_typ = top rv_pl in
              unify_typs_lfts pl_typ rv_pl_typ
          | RVbinop (_, lpl, rpl) ->
              let lpl_typ, rpl_typ = top lpl, top rpl in
              unify_typs_lfts lpl_typ rpl_typ;
              unify_typs_lfts lpl_typ pl_typ;
              unify_typs_lfts rpl_typ pl_typ
          | RVborrow (_, rv_pl) ->
              let rec add_reborrow_constraint = function
                (* If the rvalue is a reborrow, we must add outlive relations between the
                   lvalue's lifetime and underlying rvalue's borrows lifetimes. *)
                | PlDeref rv_pl -> (
                    match get_lft (top pl), get_lft (top rv_pl) with
                    | Some pl_lft, Some rv_pl_lft -> add_outlives (rv_pl_lft, pl_lft)
                    | _ ->
                        ();

                        add_reborrow_constraint rv_pl)
                (* If the rvalue is a borrow, we must unify its lifetime with the lvalue's
                   one *)
                | _ -> unify_typs_lfts (top pl) (top rv_pl)
              in
              add_reborrow_constraint rv_pl
          (* If the rvalue is composed, we must unify every types's lifetimes composing it
             with the lvalue's components lifetimes. *)
          | RVmake (sid, rv_pls) ->
              let fields_typs, struct_typ = fields_types_fresh prog sid in

              (* We unify fresh lifetimes with the rvalue's ones *)
              List.iter2
                (fun arg_pl field_typ -> unify_typs_lfts (top arg_pl) field_typ)
                rv_pls fields_typs;

              (* The we unify the lvalue's lifetimes with the fresh ones *)
              unify_typs_lfts (top pl) struct_typ
          (* If the rvalue is a constant, we do not need to unify any lifetimes. *)
          | RVconst _ | RVunit -> ())
      | Icall (fid, params_pls, ret, _) ->
          let params_typs, ret_typ, outlives = fn_prototype_fresh prog fid in

          (* We add the outlives relations of the function. *)
          List.iter (fun olr -> add_outlives olr) outlives;

          (* Then we unify fresh parameters' lifetimes with parameters' lifetimes *)
          List.iter2
            (fun param_pl param_typ -> unify_typs_lfts (top param_pl) param_typ)
            params_pls params_typs;

          (* And we unify the return's lifetime with the fresh return lifetime *)
          unify_typs_lfts (top ret) ret_typ)
    mir.minstrs;

  (* Task n°3 : END ********************************************************************)

  (* The [living] variable contains constraints of the form "lifetime 'a should be
    alive at program point p". *)
  let living : PpSet.t LMap.t ref = ref LMap.empty in

  (* Helper function to add living constraint. *)
  let add_living pp l =
    living :=
      LMap.update l
        (fun s -> Some (PpSet.add pp (Option.value s ~default:PpSet.empty)))
        !living
  in

  (* Run the live local analysis. See module Live_locals for documentation. *)
  let live_locals = Live_locals.go mir in

  (* TODO: generate living constraints:
     - Add living constraints corresponding to the fact that liftimes appearing free
       in the type of live locals at some program point should be alive at that
       program point.
     - Add living constraints corresponding to the fact that generic lifetime variables
       (those in [mir.mgeneric_lfts]) should be alive during the whole execution of the
       function.
  *)

  (* Task n°4 : BEGIN ******************************************************************)
  Array.iteri
    (fun label _ ->
      (* We add living constraints for free lifetimes of live locals *)
      LocSet.iter
        (fun loc ->
          LSet.iter
            (fun fl -> add_living (PpLocal label) fl)
            (free_lfts (typ_of_place prog mir (PlLocal loc))))
        (live_locals label);

      (* Then we add the living constraints for generic lifetimes *)
      List.iter (fun gl -> add_living (PpLocal label) gl) mir.mgeneric_lfts)
    mir.minstrs;

  (* Task n°4 : END ********************************************************************)

  (* If [lft] is a generic lifetime, [lft] is always alive at [PpInCaller lft]. *)
  List.iter (fun lft -> add_living (PpInCaller lft) lft) mir.mgeneric_lfts;

  (* Now, we compute lifetime sets by finding the smallest solution of the constraints, using the
    Fix library. *)
  let module Fix = Fix.Fix.ForType (struct type t = lifetime end) (Fix.Prop.Set (PpSet))
  in
  Fix.lfp (fun lft lft_sets ->
      LSet.fold
        (fun lft acc -> PpSet.union (lft_sets lft) acc)
        (Option.value ~default:LSet.empty (LMap.find_opt lft !outlives))
        (Option.value ~default:PpSet.empty (LMap.find_opt lft !living)))

let borrowck prog mir =
  (* We check initializedness requirements for every instruction. *)
  let uninitialized_places = Uninitialized_places.go prog mir in
  Array.iteri
    (fun lbl (instr, loc) ->
      let uninit : PlaceSet.t = uninitialized_places lbl in

      let check_initialized pl =
        if PlaceSet.exists (fun pluninit -> is_subplace pluninit pl) uninit then
          Error.error loc "Use of a place which is not fully initialized at this point."
      in

      (match instr with
      | Iassign (pl, _, _) | Icall (_, _, pl, _) -> (
          match pl with
          | PlDeref pl0 ->
              if PlaceSet.mem pl0 uninit then
                Error.error loc "Writing into an uninitialized borrow."
          | PlField (pl0, _) ->
              if PlaceSet.mem pl0 uninit then
                Error.error loc "Writing into a field of an uninitialized struct."
          | _ -> ())
      | _ -> ());

      match instr with
      | Iassign (_, RVplace pl, _) | Iassign (_, RVborrow (_, pl), _) ->
          check_initialized pl
      | Iassign (_, RVbinop (_, pl1, pl2), _) ->
          check_initialized pl1;
          check_initialized pl2
      | Iassign (_, RVunop (_, pl), _) | Iif (pl, _, _) -> check_initialized pl
      | Iassign (_, RVmake (_, pls), _) | Icall (_, pls, _, _) ->
          List.iter check_initialized pls
      | Ireturn -> check_initialized (PlLocal Lret)
      | Iassign (_, (RVunit | RVconst _), _) | Ideinit _ | Igoto _ -> ())
    mir.minstrs;

  (* We check the code honors the non-mutability of shared borrows. *)

  (* Task n°2 : BEGIN ******************************************************************)
  let err loc = Error.error loc "Creating a mutable borrow below a shared borrow" in

  let is_mut_bor pl =
    match typ_of_place prog mir pl with Tborrow (_, Mut, _) -> true | _ -> false
  in

  let is_shared_bor pl =
    let rec typ_loop = function
      | Tborrow (_, Mut, typ) -> typ_loop typ
      | Tborrow (_, NotMut, typ) -> true
      | _ -> false
    in
    let rec pl_loop pl =
      match pl with
      | PlDeref pl | PlField (pl, _) -> typ_loop (typ_of_place prog mir pl) || pl_loop pl
      | _ -> typ_loop (typ_of_place prog mir pl)
    in
    pl_loop pl
  in

  Array.iteri
    (fun _ (instr, loc) ->
      (* Task n°2 : OK *)
      match instr with
      (* A user could try to create a mutable borrow of a shared borrow using an assignment *)
      | Iassign (pl, rv, _) -> (
          (* I first check that we are not writing into a shared borrow *)
          if place_mut prog mir pl = NotMut then
            Error.error loc "Writing into a shared borrow";

          (* Then I check that we are not creating a mutable borrow below a shared borrow *)
          match rv with
          | RVplace rv_pl -> if is_mut_bor pl && is_shared_bor rv_pl then err loc
          | RVborrow (mut, rv_pl) ->
              if is_mut_bor pl && (is_shared_bor rv_pl || mut = NotMut) then err loc
          (* Binop, unop, const and unit only work with POD types and make can not directly create
             mutable references (as the language does not support fields typed as borrows). *)
          | _ -> ())
      (* A user could either try to create a mutable borrow of a shared borrow through function
         arguments in a call *)
      | Icall (fun_name, args_pls, ret_pl, _) ->
          let fun_def = Ast.get_fun_def prog fun_name in
          List.iter2
            (fun (_, mut, typ) arg_pl ->
              if
                is_shared_bor arg_pl
                && (mut = Mut || match typ with Tborrow (_, Mut, _) -> true | _ -> false)
              then err loc)
            fun_def.fformals args_pls
      | _ -> ())
    mir.minstrs;

  (* Task n°2 : END ********************************************************************)
  let lft_sets = compute_lft_sets prog mir in

  (* TODO: check that outlives constraints declared in the prototype of the function are
    enough to ensure safety. I.e., if [lft_sets lft] contains program point [PpInCaller lft'], this
    means that we need that [lft] be alive when [lft'] dies, i.e., [lft'] outlives [lft]. This relation
    has to be declared in [mir.outlives_graph]. *)

  (* Task n°4 : BEGIN ******************************************************************)
  let err lft lft' =
    Error.error mir.mloc
      "Explicit outlive relation '%s <: %s' not provided in function prototype"
      (match lft with Ast.Lnamed s -> s | _ -> "")
      (match lft' with Ast.Lnamed s -> s | _ -> "")
  in

  List.iter
    (fun lft ->
      PpSet.iter
        (function
          | PpInCaller lft' -> (
              match LMap.find_opt lft mir.moutlives_graph with
              | Some lfts ->
                  if not (LSet.exists (fun l -> l = lft') lfts) then err lft lft'
              | None -> err lft lft')
          | _ -> ())
        (lft_sets lft))
    mir.mgeneric_lfts;

  (* Task n°4 : END ********************************************************************)

  (* We check that we never perform any operation which would conflict with an existing
    borrows. *)
  let bor_active_at = Active_borrows.go prog lft_sets mir in
  Array.iteri
    (fun lbl (instr, loc) ->
      (* The list of bor_info for borrows active at this instruction. *)
      let active_borrows_info : bor_info list =
        List.map (get_bor_info prog mir) (BSet.to_list (bor_active_at lbl))
      in

      (* Does there exist a borrow of a place pl', which is active at program point [lbl],
        such that a *write* to [pl] conflicts with this borrow?

         If [pl] is a subplace of pl', then writing to [pl] is always conflicting, because
        it is aliasing with the borrow of pl'.

         If pl' is a subplace of [pl], the situation is more complex:
           - if pl' involves as many dereferences as [pl] (e.g., writing to [x.f1] while
            [x.f1.f2] is borrowed), then the write to [pl] will overwrite pl', hence this is
            conflicting.
           - BUT, if pl' involves more dereferences than [pl] (e.g., writing to [x.f1] while
            [*x.f1.f2] is borrowed), then writing to [pl] will *not* modify values accessible
            from pl'. Hence, subtlely, this is not a conflict. *)
      let conflicting_borrow_no_deref pl =
        List.exists
          (fun bi -> is_subplace pl bi.bplace || is_subplace_no_deref bi.bplace pl)
          active_borrows_info
      in

      (match instr with
      | Iassign (pl, _, _) | Icall (_, _, pl, _) ->
          if conflicting_borrow_no_deref pl then
            Error.error loc "Assigning a borrowed place."
      | Ideinit (l, _) ->
          if conflicting_borrow_no_deref (PlLocal l) then
            Error.error loc
              "A local declared here leaves its scope while still being borrowed."
      | Ireturn ->
          Hashtbl.iter
            (fun l _ ->
              match l with
              | Lparam p ->
                  if conflicting_borrow_no_deref (PlLocal l) then
                    Error.error loc
                      "When returning from this function, parameter `%s` is still \
                       borrowed."
                      p
              | _ -> ())
            mir.mlocals
      | _ -> ());

      (* Variant of [conflicting_borrow_no_deref]: does there exist a borrow of a place pl',
        which is active at program point [lbl], such that a *read* to [pl] conflicts with this
        borrow? In addition, if parameter [write] is true, we consider an operation which is
        both a read and a write. *)
      let conflicting_borrow write pl =
        List.exists
          (fun bi ->
            (bi.bmut = Mut || write)
            && (is_subplace pl bi.bplace || is_subplace bi.bplace pl))
          active_borrows_info
      in

      (* Check a "use" (copy or move) of place [pl]. *)
      let check_use pl =
        let consumes = not (typ_is_copy prog (typ_of_place prog mir pl)) in
        if conflicting_borrow consumes pl then
          Error.error loc "A borrow conflicts with the use of this place.";
        if consumes && contains_deref_borrow pl then
          Error.error loc "Moving a value out of a borrow."
      in

      (* Task n°5 : BEGIN **************************************************************)
      match instr with
      | Iassign (_, RVunop (_, pl), _) -> check_use pl
      | Iassign (_, RVbinop (_, lpl, rpl), _) ->
          check_use lpl;
          check_use rpl
      | Iassign (_, RVmake (_, pls), _) -> List.iter check_use pls
      | Iassign (_, RVborrow (mut, pl), _) ->
          if conflicting_borrow (mut = Mut) pl then
            Error.error loc "There is a borrow conflicting this borrow."
      | Iassign (_, RVplace pl, _) ->
          if conflicting_borrow false pl then
            Error.error loc "There is a borrow conflicting this borrow.";

          check_use pl
      | Iassign (pl, rv, _) ->
          if conflicting_borrow_no_deref pl then
            Error.error loc "There is a borrow conflicting this borrow.";

          if conflicting_borrow true pl then
            Error.error loc "There is a borrow conflicting this borrow."
      | Ideinit (l, _) -> ()
      | _ -> ())
    (* Task n°5 : END ****************************************************************)
    mir.minstrs
