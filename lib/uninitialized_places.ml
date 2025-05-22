(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-26-27-33"]

(* You should read and understand active_borrows.ml *fully*, before filling the holes
  in this file. The analysis in this file follows the same structure. *)

open Type
open Minimir

type analysis_results = label -> PlaceSet.t

let go prog mir : analysis_results =
  (* The set of all places appearing in the MIR code. We are not interesting in initializedness for places
    which are not member of this set. *)
  let all_places =
    let acc =
      Hashtbl.fold
        (fun l _ acc -> PlaceSet.add (PlLocal l) acc)
        mir.mlocals PlaceSet.empty
    in
    Array.fold_left
      (fun acc (i, _) ->
        match i with
        | Ideinit _ | Igoto _ | Ireturn -> acc
        | Iif (pl, _, _) -> PlaceSet.add pl acc
        | Icall (_, pls, pl, _) -> PlaceSet.add_seq (Seq.cons pl (List.to_seq pls)) acc
        | Iassign (pl, rv, _) -> (
            let acc = PlaceSet.add pl acc in
            match rv with
            | RVplace pl | RVborrow (_, pl) | RVunop (_, pl) -> PlaceSet.add pl acc
            | RVbinop (_, pl1, pl2) -> PlaceSet.add pl1 (PlaceSet.add pl2 acc)
            | RVmake (_, pls) -> PlaceSet.add_seq (List.to_seq pls) acc
            | RVunit | RVconst _ -> acc))
      acc mir.minstrs
  in

  (* The set of subplaces of a given place. *)
  let subplaces = Hashtbl.create 7 in
  let () =
    PlaceSet.iter
      (fun pl ->
        let pls = PlaceSet.filter (fun pl_sub -> is_subplace pl_sub pl) all_places in
        Hashtbl.add subplaces pl pls)
      all_places
  in

  (* Effect of initializing a place [pl] on the abstract state [state]: [pl] and all its subplaces
    become initialized. Hence, given that the state is the set of uninitialized places, we remove
    the subplaces [pl] from the abstract state.

    It is important to understand that places can be nested. In order to make a place uninitialized,
    it is *not* enough to remove it from the state. One should also remove subplaces. *)
  let initialize pl state = PlaceSet.diff state (Hashtbl.find subplaces pl) in

  (* This is the dual: we are consuming or deinitiailizing place [pl], so all its subplaces
    become uninitialized, so they are added to [state]. *)
  let deinitialize pl state = PlaceSet.union state (Hashtbl.find subplaces pl) in

  (* Effect of using (copying or moving) a place [pl] on the abstract state [state]. *)
  let move_or_copy pl state =
    (* Task n°1 : BEGIN ****************************************************************)
    if typ_is_copy prog (typ_of_place prog mir pl) then initialize pl state
    else deinitialize pl state
    (* Task n°1 : END ******************************************************************)
  in

  (* These modules are parameters of the [Fix.DataFlow.ForIntSegment] functor below. *)
  let module Instrs = struct let n = Array.length mir.minstrs end in
  let module Prop = struct
    type property = PlaceSet.t

    let leq_join p q = if PlaceSet.subset p q then q else PlaceSet.union p q
  end in
  let module Graph = struct
    type variable = int
    type property = PlaceSet.t

    (* To complete this module, one can read file active_borrows.ml, which contains a
      similar data flow analysis. *)

    (* Task n°1 : BEGIN ****************************************************************)

    let foreach_root go =
      (* We initialize each parameter of the function. I'm not sure this function is
         necessary but since it does affect the results I leave it here. *)
      go mir.mentry
        (Hashtbl.fold
           (fun loc typ pls ->
             match loc with Lparam _ -> initialize (PlLocal loc) pls | _ -> pls)
           mir.mlocals all_places)

    let foreach_successor lbl uplaces go =
      (* OK *)
      match fst mir.minstrs.(lbl) with
      | Ireturn -> ()
      | Igoto next -> go next uplaces
      | Ideinit (loc, next) -> go next (deinitialize (PlLocal loc) uplaces)
      (* We handle each case of assignment *)
      | Iassign (pl, rv, next) ->
          let uplaces = initialize pl uplaces in
          let uplaces =
            match rv with
            | RVconst _ | RVunit -> uplaces
            | RVplace rv_pl | RVborrow (_, rv_pl) | RVunop (_, rv_pl) ->
                uplaces |> move_or_copy rv_pl
            | RVbinop (_, rv_left_pl, rv_right_pl) ->
                uplaces |> move_or_copy rv_right_pl |> move_or_copy rv_left_pl
            | RVmake (_, rv_pls) ->
                List.fold_right (fun rv_pl upls -> move_or_copy rv_pl upls) rv_pls uplaces
          in
          go next uplaces
      | Icall (_, args, ret, next) ->
          let uplaces' =
            uplaces
            (* The return place is either move or copied *)
            |> initialize ret
            (* Each argument is either moved or copied *)
            |> List.fold_right (fun arg_pl upls -> move_or_copy arg_pl upls) args
          in
          go next uplaces'
      | Iif (pl, next_if, next_else) ->
          let uplaces' = move_or_copy pl uplaces in
          go next_if uplaces';
          go next_else uplaces'

    (* Task n°1 : END ******************************************************************)
  end in
  let module Fix = Fix.DataFlow.ForIntSegment (Instrs) (Prop) (Graph) in
  fun i -> Option.value (Fix.solution i) ~default:PlaceSet.empty
