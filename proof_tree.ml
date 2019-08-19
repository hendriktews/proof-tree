(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2019 Hendrik Tews
 * 
 * This file is part of "prooftree".
 * 
 * "prooftree" is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * "prooftree" is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the parent
 * directories for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with "prooftree". If not, see <http://www.gnu.org/licenses/>.
 *)


(** Internal representation of proof trees with undo info 

    This module processes the proof-tree display commands that are
    read from standard input. It keeps a state record (of type
    {!proof_tree}) for all proof trees currently displayed. 
*)

open Util
open Gtk_ext
open Configuration
open Draw_tree
open Tree_layers
open Proof_window
open Evar_types
open Emacs_commands


(** Internal exception for state mismatches. This exception is raised
    when the internal state of one proof tree in this module is
    inconsistent with the proof-tree display command that has been
    read. In a correct setup (with the right Proof General version)
    such an error indicates a bug in either prooftree or in the Proof
    General preprocessing code. This exception can also be raised if
    prooftree is used with a wrong version of Proof General or if
    somebody manipulates the internal data structures of Proof
    General.
*)
exception Proof_tree_error of string


(******************************************************************************
 *****************************************************************************)
(** {2 Record for managing proof trees} *)
(*****************************************************************************)

type proof_tree = {
  window : proof_window;
  (** The window displaying this tree. *)

  proof_name : string; 
  (** The name of the proof *)

  mutable pa_start_state : int;
  (** Internal proof assistant state number where this proof starts. 
      Used to detect bulk undos.
  *)

  mutable pa_end_state : int;
  (** Internal proof assistant state number where this proof finishes,
      or [-1] if this proof is not finished yet. Used to speed up undo
      processing.
  *)

  mutable cheated : bool;
  (** [true] if a cheating command has been used somewhere in the proof. *)

  sequent_hash : (string, turnstile) Hashtbl.t;
  (** Hash table mapping all currently known sequents of this proof
      tree to {!class: Draw_tree.turnstile} objects. Used to detect new
      sequents and to update sequents.
  *)

  mutable current_sequent_id : string option;
  (** The ID of the current sequent, if there is one. Needed to
      distinguish the peculiar case, where a non-failing proof command
      (such as [auto]) does not change the proof state. 
  *)

  mutable current_sequent : proof_tree_element option;
  (** The object of the current sequent, if there is one. Used for
      coloring branches. There is no current sequent, if a branch has
      been finished without switching to a new goal.
  *)

  mutable open_goals_count : int;
  (** Counter for the total number of open goals, including the
      current sequent.
  *)

  existential_hash : (string, existential_variable) Hashtbl.t;
  (** Mapping containing all existential variables in the proof tree.
      Needed to establish the dependency links in instantiated
      existentials. 
  *)

  mutable need_redraw : bool;
  (** [true] if the tree display needs a redraw. Used to delay redrawing. *)

  mutable sequent_area_needs_refresh : bool;
  (** [true] if the sequent area needs to be refreshed. Used to delay
      sequent area refreshs.
  *)

  mutable undo_actions : (unit -> unit) list Int_map.t
(** List of undo actions for this proof tree. Each element has the
    form [(state, action_list)], where [action_list] is the list of
    undo actions that must be performed if the user retracts to a
    state equal or lesser than [state].
*)
}
(** State record for displayed proof trees. The code maintains the
    following invariants.
    {ul 
    {- Each displayed proof tree is in precisely one the lists
    {!original_proof_trees} or
    {!Proof_window.cloned_proof_windows}.}
    {- {!proof_tree.current_sequent_id} = [None] iff
    {!proof_tree.current_sequent} = [None]}
    }
*)

(** Contains all non-cloned proof trees managed in this module.
    Cloned proof trees are in {!Proof_window.cloned_proof_windows}.
*)
let original_proof_trees = ref []

(** Holds the state for the currently active proof window, if any.
    Mainly used for {!finish_drawing} to delay redrawing.
*)
let current_proof_tree = ref None


(** Try to find a proof window for [proof_name].
*)
let find_proof_tree_by_name proof_name =
  List.find_opt (fun pt -> pt.proof_name = proof_name) !original_proof_trees


(** Create a new proof-tree state (containing an empty proof tree
    window) for [proof_name] with starting state [state].
*)
let create_new_proof_tree proof_name state =
  {
    window = make_proof_window proof_name !geometry_string;
    proof_name = proof_name;
    pa_start_state = state;
    pa_end_state = -1;
    cheated = false;
    sequent_hash = Hashtbl.create 503;
    current_sequent_id = None;
    current_sequent = None;
    open_goals_count = 0;
    existential_hash = Hashtbl.create 251;
    need_redraw = true;
    sequent_area_needs_refresh = true;
    undo_actions = Int_map.empty;
  }


(** Initialize a surviver proof-tree state (and window) for reuse with
    the initial sequent [current_sequent] and start state [state].
*)
let reinit_surviver pt state =
  pt.window#clear_for_reuse;
  Hashtbl.clear pt.sequent_hash;
  Hashtbl.clear pt.existential_hash;
  pt.pa_start_state <- state;
  pt.pa_end_state <- -1;
  pt.cheated <- false;
  pt.current_sequent_id <- None;
  pt.current_sequent <- None;
  pt.open_goals_count <- 0;
  pt.need_redraw <- true;
  pt.sequent_area_needs_refresh <- true;
  pt.undo_actions <- Int_map.empty;
  pt.window#message ""


(******************************************************************************
 *****************************************************************************)
(** {2 Evar Record treatment} *)
(*****************************************************************************)

(** Mark the given existential as instantiated, link it with its
   dependencies and initiate sequents updates for all sequents
   containing the existential.
*)
let instantiate_existential proof_name state ex_hash ex dependency_names =
  (* Printf.fprintf (debugc()) "inst %s at state %d\n%!"
   *   ex.evar_internal_name state;
   *)
  assert(ex.evar_deps = []);
  ex.evar_status <- Partially_instantiated;
  ex.evar_inst_state <- state;
  ex.evar_deps <- List.rev_map (Hashtbl.find ex_hash) dependency_names;
  Int_map.iter
    (fun _ -> List.iter (fun s -> emacs_send_show_goal proof_name state s#id))
    ex.evar_sequents


(** Reset the given list of existential variables to not being
    instantiated.
*)
let undo_instantiate_existentials exl =
  List.iter
    (fun ex -> 
      ex.evar_status <- Uninstantiated;
      ex.evar_inst_state <- -1;
      ex.evar_deps <- [];
    )
    exl


(** Create a new existential variable and add it to the hash of
    existentials. The newly created existential is returned.
*)
let make_new_existential ex_hash internal_name external_name_opt =
  let ex = {evar_internal_name = internal_name;
            evar_external_name = external_name_opt;
	    evar_status = Uninstantiated;
	    evar_mark = false;
            evar_inst_state = -1;
	    evar_deps = [];
            evar_sequents = Int_map.empty;
	   }
  in
  Hashtbl.add ex_hash internal_name ex;
  ex


(** Walk over all existential variables and update their instantiation
    status. More precisely, for evars that are instantiated (i.e.,
    have a status of [Partially_instantiated] or [Fully_instantiated],
    see {!Draw_tree.existential_status}) the complete tree of dependencies is
    scanned and then their status is set appropriately.
*)
let update_existential_status ex_hash =
  let visited_hash = Hashtbl.create 251 in
  let rec collect ex = 
    if Hashtbl.mem visited_hash ex.evar_internal_name
    then ()
    else begin
      if ex.evar_status <> Uninstantiated
      then begin
	List.iter collect ex.evar_deps;
	ex.evar_status <-
    	  if (List.for_all (fun dep -> dep.evar_status = Fully_instantiated)
		ex.evar_deps)
    	  then Fully_instantiated
    	  else Partially_instantiated
      end;
      Hashtbl.add visited_hash ex.evar_internal_name ()
    end
  in
  Hashtbl.iter (fun _ ext -> collect ext) ex_hash



(** Update the hash of existential variables and the existentials
    themselves. The [evar_info] is scanned for new existentials.
    Note that new existentials can even be found in the dependencies
    of instantiated existentials, because the order inside [evar_info] may 
    be arbitrary and also because some complex strategy might create and
    instantiate several existentials. Newly created existentials are
    registered in the hash of existential variables. Finally the state
    of those existentials that got instantiated is updated. 

    This function returns a triple containing the list of new existentials,
    the list of newly instantiated existentials and the [current_goal_evars]
    argument filtered for open evars.

    Argument [state] is the proof assistant state from which the information
    about existential variables was collected. It is needed for triggering
    sequent updates for sequents containing newly instantiated evars.
*)
let update_existentials proof_name state ex_hash evar_info current_goal_evars =
  (* Create one existential_variable record for ex_name, if ex_name is new
   * and append it to accumulator argument accu_new. We might see an evar
   * in the dependency list, before we see that it is uninstantiated and
   * see its external name. We therefore update the external name here.
   *)
  let test_and_create_ex accu_new int_name ex_name =
    match Hashtbl.find_opt ex_hash int_name with
      | Some ex ->
         if ex_name <> None
         then ex.evar_external_name <- ex_name;
         accu_new
      | None -> (make_new_existential ex_hash int_name ex_name) :: accu_new
  in
  let process_evar_info (accu_new, accu_inst) = function
    | Noninstantiated(int_name, ex_name)->
        ((test_and_create_ex accu_new int_name (Some ex_name)), accu_inst)
    | Instantiated(int_name, deps) ->
        (* Complex stategies might create and instantiate several
         * existentials. It may therefore happen that some
         * instantiated existential and even some of its dependencies
         * are actually new.
         *)
        let accu_new =
          List.fold_left
            (fun accu_new int_name -> test_and_create_ex accu_new int_name None)
            accu_new
            (int_name :: deps)
        in
        let ex = Hashtbl.find ex_hash int_name in
        let accu_inst =
          if ex.evar_status = Uninstantiated
          then begin
            instantiate_existential proof_name state ex_hash ex deps;
            ex :: accu_inst
          end
          else accu_inst
        in
        (accu_new, accu_inst)
  in
  (* XXX update mappings in evar records? Check that nothing changed? *)
  (* XXX sort the new evars? With a coq specific comparison function? *)
  let (evar_new, evar_inst) =
    List.fold_left process_evar_info ([], []) evar_info
  in
  let current_open =
    List.fold_left
      (fun current_open int_name ->
        let ex = Hashtbl.find ex_hash int_name in
        match ex.evar_status with
          | Uninstantiated ->  ex :: current_open
          | Partially_instantiated
          | Fully_instantiated -> current_open)
      []
      current_goal_evars
  in
  (evar_new, evar_inst, current_open)

(** XXX *)
let filter_current_open_existentials ex_hash evar_info goal_evars =
  let open_evars = Hashtbl.create 57 in
  List.iter
    (function
     | Noninstantiated(int_name, _) -> Hashtbl.add open_evars int_name true
     | Instantiated _ -> ())
    evar_info;
  List.rev_map
    (Hashtbl.find ex_hash)
    (list_filter_rev (Hashtbl.mem open_evars) goal_evars)


(** XXX *)
let evar_register_sequent proof_name state sequent evar =
  (* Printf.fprintf (debugc()) "register %s at %s\n%!"
   *   sequent#id evar.evar_internal_name;
   *)
  evar.evar_sequents <-
    Int_map.update state
      (function
       | None -> Some [sequent]
       | Some seqs -> Some (sequent :: seqs))
      evar.evar_sequents;
  match evar.evar_status with
    | Uninstantiated -> ()
    | Partially_instantiated
    | Fully_instantiated ->
       emacs_send_show_goal proof_name evar.evar_inst_state sequent#id

(** XXX *)
let undo_evar_sequents undo_state ex =
  let (seq_less, seq_equal, _) =
    Int_map.split undo_state ex.evar_sequents in
  ex.evar_sequents <-
    match seq_equal with
      | None -> seq_less
      | Some s -> Int_map.add undo_state s seq_less


(******************************************************************************
 *****************************************************************************)
(** {2 Undo} *)
(*****************************************************************************)

(** Local convenience function for changing the current node. Sets the
    current node in the proof-tree window and schedules an update for the
    sequent area if there is no selected node.
*)
let set_current_node_wrapper pt sequent =
  (match sequent with
     | Some s -> pt.window#set_current_node s;
     | None -> pt.window#clear_current_node;
  );
  if pt.window#get_selected_node = None then
    pt.sequent_area_needs_refresh <- true

(** Local convenience function for marking the current sequent. *)
let mark_current_sequent_maybe = function
  | Some cs -> cs#mark_current
  | None -> ()


(** Add an undo action to the current state [pa_state] of the proof
    [pt]. This action is performed if the user retracts to a state
    equal or lesser than [pa_state].
*)
let add_undo_action pt pa_state undo_fun =
  pt.undo_actions <-
    Int_map.update pa_state
      (function
       | None -> Some [undo_fun]
       | Some fs -> Some (undo_fun :: fs))
      pt.undo_actions

(** Closes the proof tree [pt] by leaving the current branch open.
    Additionally clear {!current_proof_tree}.
*)
let stop_proof_tree pt pa_state = 
  (* Don't keep undo actions. Because of insertions, undo state
   * numbers might get out of sync with retired proof trees. Undo into
   * the middle of a proof is therefore impossible.
   *)
  pt.undo_actions <- Int_map.empty;
  pt.pa_end_state <- pa_state;
  pt.window#disconnect_proof;
  pt.window#clear_current_node;
  pt.window#refresh_sequent_area;
  update_existential_status pt.existential_hash;
  pt.window#refresh_and_position;
  pt.window#update_ext_dialog;
  pt.need_redraw <- false;
  pt.sequent_area_needs_refresh <- false;
  current_proof_tree := None


(** Same as {!stop_proof_tree} but make the current sequent the
    selected one, if there is no selected sequent.
*)
let stop_proof_tree_last_selected pt pa_state =
  (match pt.window#get_selected_node with
    | None -> 
      (* stop_proof_tree will clear all undo actions
       * add_undo_action pt pa_state
       * 	(fun () -> pt.window#set_selected_node None);
       *)
      pt.window#select_root_node
    | Some _ -> ());
  stop_proof_tree pt pa_state


(** Result values for [undo_tree] that tell the calling function
    [undo] what to do with the argument proof tree.
*)
type proof_tree_undo_result =
  | PT_undo_delete    (** Argument proof tree should be deleted  *)
  | PT_undo_current   (** Argument proof tree should be kept as current *)
  | PT_undo_keep      (** Argument proof tree should be kept non-current *)


(** Process all undo actions with a state strictly greater than
   [undo_state]. Return the map of unprocessed undo actions (with
   state less than or equal to [undo_state]).
 *)
let fire_undo_actions undo_state undo_map =
  let (undos_less, undos_equal, undos_greater) =
    Int_map.split undo_state undo_map in
  let undos_left = match undos_equal with
      | None -> undos_less
      | Some fs -> Int_map.add undo_state fs undos_less
  in
  (* Need to process the undo actions in descending order, but the map
     only iterates in ascending order. *)
  List.iter (fun fs -> List.iter (fun f -> f()) fs)
    (Int_map.fold (fun _ fs fss -> fs :: fss) undos_greater []);
  undos_left

(** XXX *)
let undo_inside_tree pt pa_state =
  pt.undo_actions <- fire_undo_actions pa_state pt.undo_actions;
  Hashtbl.iter
    (fun _ ex -> undo_evar_sequents pa_state ex)
    pt.existential_hash

(** Perform undo actions in proof tree [pt] to reach state [pa_state].
    This means that either 
    {ul
    {- nothing is done (because [pt] was finished in a state less than
    [pa_state]),}
    {- some of the nodes in [pt] are deleted,}
    {- the complete window belonging to [pt] is deleted, or}
    {- no node of [pt] is deleted and pt is kept as surviver (because
    a bulk undo with a state less than the starting state of [pt] was
    detected.)}
    }
    Because of insertions, undo-state numbers might get out of sync
    with retired proof trees. Therefore, undo into the middle of an
    retired proof tree is not supported.
*)
let undo_tree pt pa_state =
  let pt_is_current = match !current_proof_tree with
    | Some cpt -> pt == cpt
    | None -> false
  in
  if pa_state < pt.pa_start_state
  then begin
    if pt.window#survive_undo_before_start 
    then begin
      pt.window#message "Retract before start";
      if pt_is_current then
	stop_proof_tree pt (-1)
      else 
	pt.pa_end_state <- -1;
      pt.window#clear_position_hints;
      PT_undo_keep
    end
    else begin
      pt.window#delete_proof_window;
      PT_undo_delete
    end
  end
  else if pt.pa_end_state >= 0 && pa_state >= pt.pa_end_state 
  then begin
    assert(pt_is_current = false);
    PT_undo_keep
  end
  else (* pt.pa_start_state <= pa_state < pt.pa_end_state *)
    if pt_is_current 
    then
      begin
        undo_inside_tree pt pa_state;
	mark_current_sequent_maybe pt.current_sequent;
	set_current_node_wrapper pt pt.current_sequent;
	pt.window#message (Printf.sprintf "Retract to %d" pa_state);
	pt.need_redraw <- true;
	pt.window#clear_position_hints;
	PT_undo_current
      end
    else 
      begin
	if pt.window#survive_undo_before_start 
	then begin
	  pt.window#message "Retract before start";
	  pt.pa_end_state <- -1;
	  pt.window#clear_position_hints;
	  PT_undo_keep
	end
	else begin
	  pt.window#delete_proof_window;
	  PT_undo_delete
	end
      end 


(** Perform undo to state [pa_state] in all non-cloned proof trees
    ({!original_proof_trees}). As result some of the proof windows
    might get deleted, some proof trees might get changed, and some
    might be kept as surviver. {!current_proof_tree} might be cleared.
*)
let undo pa_state =
  let new_current = ref None in
  original_proof_trees :=
    List.fold_left
    (fun pts pt -> match undo_tree pt pa_state with
      | PT_undo_delete -> pts
      | PT_undo_current -> 
	new_current := Some pt;
	pt :: pts
      | PT_undo_keep -> pt :: pts
    )
    [] !original_proof_trees;
  current_proof_tree := !new_current


(******************************************************************************
 *****************************************************************************)
(** {2 Proof tree manipulations} *)
(*****************************************************************************)

(** Start a new proof [proof_name] which is initially empty, that is
    contains no proof tree layers. If a surviver proof tree is found
    it is reused. Otherwise a new proof-tree state and window is
    created.
*)
let start_new_proof state proof_name =
  let pt =
    match find_proof_tree_by_name proof_name with
      | None -> 
	let pt = create_new_proof_tree proof_name state in
	original_proof_trees := pt :: !original_proof_trees;
	pt
      | Some pt -> 
	reinit_surviver pt state;
	pt
  in
  current_proof_tree := Some pt;
  pt


(** Create a new layer in the proof tree display with the current
    sequent and all additional sequents as root goals. There must be
    no open subgoal. The information about existential variables is
    processed, but there must be no new and no newly instantiated
    existential variables. If this is the first layer in the display,
    a warning is displayed, if there is more than 1 root node.
*)
let create_new_layer pt state current_sequent_id current_sequent_text
    additional_ids evar_info current_evar_names =
  assert(List.for_all (fun id -> Hashtbl.mem pt.sequent_hash id = false)
	   (current_sequent_id :: additional_ids));
  assert(pt.open_goals_count = 0);
  let (new_evars, inst_evars, _) =
    update_existentials pt.proof_name state
      pt.existential_hash evar_info current_evar_names in
  (* Printf.fprintf (debugc())
   *   "new layer %s at state %d, inst %s, this goal open none\n%!"
   *   current_sequent_id
   *   state
   *   (String.concat " "
   *      (List.rev_map (fun e -> e.evar_internal_name) inst_evars));
   *)
  (* schedule an existential status update when this assert goes away *)
  assert (new_evars = [] && inst_evars = [] && current_evar_names = []);
  let first_sw =
    pt.window#new_turnstile state current_sequent_id 
      (Some current_sequent_text) in
  Hashtbl.add pt.sequent_hash current_sequent_id first_sw;
  let first_sw = (first_sw :> proof_tree_element) in
  let other_sw =
    List.fold_right
      (fun id res ->
	let sw = pt.window#new_turnstile state id None in
	Hashtbl.add pt.sequent_hash id sw;
	(sw :> proof_tree_element) :: res)
      additional_ids []
  in
  let all_roots = first_sw :: other_sw in
  let layer = new tree_layer all_roots in
  let layer_undo_pos = pt.window#layer_stack#add_layer layer in
  pt.current_sequent_id <- Some current_sequent_id;
  pt.current_sequent <- Some first_sw;
  pt.open_goals_count <- List.length all_roots;
  first_sw#mark_current;
  set_current_node_wrapper pt (Some first_sw);
  pt.window#clear_position_hints;
  let layer_count = pt.window#layer_stack#count_layers in
  let message =
    if layer_count = 1
    then "Initial sequent"
    else 
      Printf.sprintf "Layer %d with %d goals" layer_count pt.open_goals_count
  in
  pt.window#message message;
  let unhash_sequent_ids = current_sequent_id :: additional_ids in
  let undo () =
    List.iter (fun s -> s#delete_non_sticky_external_windows) all_roots;
    List.iter (fun id -> Hashtbl.remove pt.sequent_hash id) unhash_sequent_ids;
    List.iter 
      (fun sw -> if sw#is_selected then pt.window#set_selected_node None)
      all_roots;
    pt.window#layer_stack#del_layer layer_undo_pos;
    pt.current_sequent_id <- None;
    pt.current_sequent <- None;
    pt.open_goals_count <- 0;
  in
  add_undo_action pt state undo;
  pt.need_redraw <- true;
  pt.sequent_area_needs_refresh <- true;
  if layer_count = 1 && pt.open_goals_count > 1 then
    run_message_dialog
      "More than one initial goal. \n\
       You need to start proofs with \n\
       the \"Proof\" command!"
      `WARNING


(** Add a new proof command with the new current sequent
    [current_sequent] and the additionally spawned subgoals. The
    additionally spawned subgoals are computed from [additional_ids]
    which must contain the ID's of all new subgoals (except for
    [current_sequent_id]). Old, currently unfinished subgoals in
    [additional_ids] are filtered out with the help of
    [pt.sequent_hash]. Except for the [current_sequent], the newly
    created subgoals contain no sequent text yet. This is expected to
    arrive soon with an [update-sequent] command.

    [cheated_flag] is asserted to be false, because the code assumes
    that a cheating command solves the current subgoal.
*)
let add_new_goal pt state proof_command cheated_flag current_sequent_id
    current_sequent_text additional_ids evar_info current_evar_names =
  assert(cheated_flag = false);
  let (new_evars, inst_evars, current_open_evars) =
    update_existentials pt.proof_name state
      pt.existential_hash evar_info current_evar_names in
  (* Printf.fprintf (debugc())
   *   "new goal %s at state %d, inst %s, this goal open %s\n%!"
   *   current_sequent_id
   *   state
   *   (String.concat " "
   *      (List.rev_map (fun e -> e.evar_internal_name) inst_evars))
   *   (String.concat " "
   *      (List.rev_map (fun e -> e.evar_internal_name) current_open_evars));
   *)
  (* Update the existentials early, to have correct info in the 
   * current sequent.
   *)
  if inst_evars <> [] then
    update_existential_status pt.existential_hash;
  let parent = match pt.current_sequent with
    | Some s -> s
    | None -> assert false
  in
  let pc = 
    pt.window#new_proof_command state proof_command new_evars inst_evars
  in
  let pc = (pc :> proof_tree_element) in
  set_children parent [pc];
  let sw =
    pt.window#new_turnstile state current_sequent_id 
      (Some current_sequent_text) in
  Hashtbl.add pt.sequent_hash current_sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  let new_goal_ids_rev = 
    list_filter_rev 
      (fun id -> not (Hashtbl.mem pt.sequent_hash id))
      additional_ids in
  List.iter (emacs_send_show_goal pt.proof_name state) new_goal_ids_rev;
  let new_goals =
    List.fold_left
      (fun res id ->
	let sw = pt.window#new_turnstile state id None in
	Hashtbl.add pt.sequent_hash id sw;
	let sw = (sw :> proof_tree_element) in
	sw :: res)
      [] new_goal_ids_rev
  in
  let position_hints = match new_goals with
    | [] -> [[pc]]
    | [snd] -> [[snd; pc]; [snd]; [pc]]
    | snd :: rest -> 
      let last = list_last rest in
      [[last; pc]; [snd; pc]; [snd]; [pc]]
  in
  let all_subgoals = sw :: new_goals in
  set_children pc all_subgoals;
  List.iter
    (evar_register_sequent pt.proof_name state (sw :> turnstile_interface))
    current_open_evars;
  let unhash_sequent_ids = current_sequent_id :: new_goal_ids_rev in
  let old_current_sequent_id = pt.current_sequent_id in
  let old_current_sequent = parent in
  let old_open_goals_count = pt.open_goals_count in
  pt.current_sequent_id <- Some current_sequent_id;
  pt.current_sequent <- Some sw;
  pt.open_goals_count <- pt.open_goals_count + List.length new_goals;
  sw#mark_current;
  set_current_node_wrapper pt (Some sw);
  pt.window#set_position_hints position_hints;
  (* The uninstantiated existentials are displayed together with the
   * sequent. Therefore, if some existential got instantiated we have
   * to update all those sequent displays.
   *)
  if inst_evars <> [] then begin
    pt.window#update_sequent_existentials_info;
    pt.sequent_area_needs_refresh <- true;
  end;
  let message = match List.length new_goals with
    | 0 -> 
      Printf.sprintf "%d open goal%s (no new)" 
	pt.open_goals_count (if pt.open_goals_count > 1 then "s" else "")
    | n ->
      Printf.sprintf "%d open goal%s (%d new)"
	pt.open_goals_count (if pt.open_goals_count > 1 then "s" else "") n
  in
  pt.window#message message;
  pt.window#ext_dialog_add new_evars;
  let undo () =
    pc#delete_non_sticky_external_windows;
    List.iter (fun s -> s#delete_non_sticky_external_windows) all_subgoals;
    clear_children old_current_sequent;
    old_current_sequent#mark_current;
    List.iter (fun id -> Hashtbl.remove pt.sequent_hash id) unhash_sequent_ids;
    List.iter 
      (fun sw -> if sw#is_selected then pt.window#set_selected_node None)
      all_subgoals;
    if pc#is_selected then pt.window#set_selected_node None;
    pt.current_sequent_id <- old_current_sequent_id;
    pt.current_sequent <- Some old_current_sequent;
    pt.open_goals_count <- old_open_goals_count;
    List.iter
      (fun ex -> Hashtbl.remove pt.existential_hash ex.evar_internal_name)
      new_evars;
    if inst_evars <> [] then begin
      undo_instantiate_existentials inst_evars;
      update_existential_status pt.existential_hash;
      pt.window#update_sequent_existentials_info;
      pt.sequent_area_needs_refresh <- true;
    end;
    pt.window#ext_dialog_undo new_evars;
  in
  add_undo_action pt state undo;
  pt.need_redraw <- true


(** Add [proof_command] as final command, which solved the current
    goal, to the current branch. If [cheated_flag] is set, the branch
    is marked as cheated. This function only finishes the current
    branch, moving to the next open subgoal (if necessary) is done by
    {!internal_switch_to}.
*)
let finish_branch pt state proof_command cheated_flag evar_info
                  current_evar_names =
  (* Ignore the current open evars returned here. They might actually
   * contain something if this function is called as part of
   * finish_branch_and_switch_to.
   *)
  let (new_evars, inst_evars, _) =
    update_existentials pt.proof_name state
      pt.existential_hash evar_info current_evar_names
  in
  (* Printf.fprintf (debugc()) "branch at %d, inst %s\n%!"
   *   state
   *   (String.concat " "
   *      (List.rev_map (fun e -> e.evar_internal_name) inst_evars));
   *)
  let parent = match pt.current_sequent with
    | Some s -> s
    | None -> assert false
  in
  let pc = 
    pt.window#new_proof_command state proof_command new_evars inst_evars
  in
  let pc = (pc :> proof_tree_element) in
  parent#unmark_current;
  set_children parent [pc];
  if cheated_flag 
  then pc#mark_cheated
  else pc#mark_proved;
  let old_cheated = pt.cheated in
  let old_current_sequent = parent in
  let old_current_sequent_id = pt.current_sequent_id in
  let old_open_goals_count = pt.open_goals_count in
  let undo () =
    pc#delete_non_sticky_external_windows;
    clear_children old_current_sequent;
    old_current_sequent#unmark_proved_or_cheated;
    pt.current_sequent <- Some old_current_sequent;
    pt.current_sequent_id <- old_current_sequent_id;
    pt.open_goals_count <- old_open_goals_count;
    pt.cheated <- old_cheated;
    List.iter
      (fun ex -> Hashtbl.remove pt.existential_hash ex.evar_internal_name)
      new_evars;
    if inst_evars <> [] then begin
      undo_instantiate_existentials inst_evars;
      update_existential_status pt.existential_hash;
      pt.window#update_sequent_existentials_info;
      pt.sequent_area_needs_refresh <- true;
    end;
    pt.window#ext_dialog_undo new_evars;
  in
  add_undo_action pt state undo;
  if cheated_flag then pt.cheated <- true;
  pt.open_goals_count <- pt.open_goals_count - 1;
  pt.current_sequent <- None;
  pt.current_sequent_id <- None;
  pt.window#clear_position_hints;
  set_current_node_wrapper pt None;
  if inst_evars <> [] then begin
    update_existential_status pt.existential_hash;
    pt.window#update_sequent_existentials_info;
    pt.sequent_area_needs_refresh <- true;
  end;
  pt.window#ext_dialog_add new_evars;
  pt.need_redraw <- true


(** Switch to [new_current_sequent_id], that is, mark this sequent as
    the current one. 
*)
let internal_switch_to pt state new_current_sequent_id =
  assert(pt.current_sequent = None && pt.current_sequent_id = None);
  let new_current_sequent = 
    Hashtbl.find pt.sequent_hash new_current_sequent_id 
  in
  let new_current_sequent = (new_current_sequent :> proof_tree_element) in
  let undo () =
    new_current_sequent#unmark_current;
    pt.current_sequent_id <- None;
    pt.current_sequent <- None;
  in
  new_current_sequent#mark_current;
  set_current_node_wrapper pt (Some new_current_sequent);
  pt.current_sequent_id <- Some new_current_sequent_id;
  pt.current_sequent <- Some new_current_sequent;
  pt.window#clear_position_hints;
  add_undo_action pt state undo;
  pt.need_redraw <- true


(** Finish the current branch with [proof_command] and switch to
    [current_sequent] as next current sequent.
*)
let finish_branch_and_switch_to pt state proof_command cheated_flag
    current_sequent_id additional_ids evar_info current_evar_names =
  assert(not (List.mem current_sequent_id additional_ids));
  finish_branch pt state proof_command cheated_flag evar_info current_evar_names;
  internal_switch_to pt state current_sequent_id;
  let message = 
    Printf.sprintf "%s (%d goal%s remaining)" 
      (if cheated_flag
       then pango_markup_bold_color "Branch aborted" 
	  !cheated_gdk_color
       else pango_markup_bold_color "Branch finished" 
	  !proved_complete_gdk_color)
      pt.open_goals_count
      (if pt.open_goals_count > 1 then "s" else "")
  in
  pt.window#message message


(* See mli for doc *)
let process_current_goals state proof_name proof_command cheated_flag
    layer_flag current_sequent_id current_sequent_text additional_ids 
    evar_info current_evar_names =
  (match !current_proof_tree with
    | Some pt -> 
      if pt.proof_name <> proof_name 
      then stop_proof_tree_last_selected pt state
    | None -> ());
  let layer_flag = layer_flag || !current_proof_tree = None in
  let pt = match !current_proof_tree with
    | None -> start_new_proof state proof_name
    | Some pt ->
      assert ((iff (pt.current_sequent = None) (pt.current_sequent_id = None))
	      && (iff layer_flag (pt.current_sequent = None)));
      pt
  in
  if layer_flag
  then begin
    assert (cheated_flag = false);
    create_new_layer pt state current_sequent_id current_sequent_text
      additional_ids evar_info current_evar_names
  end else
    if pt.current_sequent_id <> (Some current_sequent_id) &&
      Hashtbl.mem pt.sequent_hash current_sequent_id
    then
      finish_branch_and_switch_to pt state proof_command cheated_flag
	current_sequent_id additional_ids evar_info current_evar_names
    else
      add_new_goal pt state proof_command cheated_flag current_sequent_id 
	current_sequent_text additional_ids evar_info current_evar_names


(** Udate the sequent text for some sequent text and set an
    appropriate undo action.
*)
let update_sequent_element pt state sw sequent_text evar_info
      current_evar_names =
  let current_open_evars =
    filter_current_open_existentials
      pt.existential_hash evar_info current_evar_names in
  (* Printf.fprintf (debugc()) "update %s for %d, this goal open %s\n%!"
   *   sw#id state
   *   (String.concat " "
   *      (List.rev_map (fun e -> e.evar_internal_name) current_open_evars));
   *)
  List.iter
    (evar_register_sequent pt.proof_name state (sw :> turnstile_interface))
    current_open_evars;
  sw#update_sequent sequent_text;
  if sw#is_selected then 
    pt.sequent_area_needs_refresh <- true;
  let undo () =
    sw#undo_update_sequent;
    if sw#is_selected then 
      pt.sequent_area_needs_refresh <- true
  in
  add_undo_action pt state undo  


(* See mli for doc *)
let update_sequent state proof_name sequent_id sequent_text
      evar_info current_evar_names =
  (* show goal commands might get arbitrarily delayed, even until
   * after the proof
   *)
  let pt_opt =
    match !current_proof_tree with
      | None -> None
      | Some pt ->
         if pt.proof_name = proof_name
         then Some pt
         else None
  in
  let pt_opt = match pt_opt with
      | Some _ -> pt_opt
      | None -> find_proof_tree_by_name proof_name
  in
  match pt_opt with
    | Some pt ->
       update_sequent_element pt state
	 (Hashtbl.find pt.sequent_hash sequent_id) sequent_text
         evar_info current_evar_names
    | None ->
       run_message_dialog
         (Printf.sprintf
            "Received update sequent command for non-existing proof %s."
             proof_name)
         `WARNING


(** Leave current branch as is to prepare for switching to a different
    goal.
*)
let leave_branch pt state =
  assert (pt.current_sequent <> None && pt.current_sequent_id <> None);
  let last_node = match pt.current_sequent with
    | Some s -> s
    | None -> assert false
  in
  last_node#unmark_current;
  let old_current_sequent = last_node in
  let old_current_sequent_id = pt.current_sequent_id in
  let undo () =
    pt.current_sequent <- Some old_current_sequent;
    pt.current_sequent_id <- old_current_sequent_id;
  in
  add_undo_action pt state undo;
  pt.current_sequent <- None;
  pt.current_sequent_id <- None;
  set_current_node_wrapper pt None;
  pt.need_redraw <- true


(* See mli for doc *)
let switch_to state proof_name new_current_sequent_id =
  match !current_proof_tree with
    | None ->
      raise (Proof_tree_error "Switch to sequent without current proof tree")
    | Some pt ->
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Switch to sequent on other proof");
      match pt.current_sequent_id with
      | Some old_id ->
	assert (pt.current_sequent <> None);
	(* Coq generates a lot switch-to commands if you use bullets
	 * and braces. Some of them don't actually change the current goal.
	 *)
	if old_id <> new_current_sequent_id then begin
	  leave_branch pt state;
	  internal_switch_to pt state new_current_sequent_id;
	  let message = 
	    Printf.sprintf "Branch changed (%d goal%s remaining)" 
	      pt.open_goals_count
	      (if pt.open_goals_count > 1 then "s" else "")
	  in
	  pt.window#message message
	end
      | None ->
	assert (pt.current_sequent = None);
	internal_switch_to pt state new_current_sequent_id;
	let message =
	  Printf.sprintf "%d open goal%s"
	    pt.open_goals_count (if pt.open_goals_count > 1 then "s" else "")
	in
	pt.window#message message


(* See mli for doc *)
let process_branch_finished state proof_name proof_command cheated_flag
      evar_info current_evar_names =
  match !current_proof_tree with
    | None -> 
      raise (Proof_tree_error "branch-finished without current proof tree")
    | Some pt -> 
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Branch finish in other proof");
      assert (pt.current_sequent <> None && pt.current_sequent_id <> None);
      finish_branch pt state proof_command cheated_flag evar_info
        current_evar_names;
      let message = 
	if pt.open_goals_count = 0
	then begin
	  let all_ex_inst = 
	    Hashtbl.fold
              (fun _ ex res -> res && ex.evar_status <> Uninstantiated)
	      pt.existential_hash true
	  in
	  let message_text =
	    (if pt.cheated then "False proof finished" else "Proof finished")
	    ^ (if all_ex_inst then "" else " (incomplete)")
	  in
	  let color = 
	    if pt.cheated then !cheated_gdk_color
	    else 
	      if all_ex_inst then !proved_complete_gdk_color
	      else !proved_incomplete_gdk_color
	  in
	  pango_markup_bold_color message_text color
	end else 
	  Printf.sprintf "%s (%d goal%s remaining)" 
	    (if cheated_flag
	     then pango_markup_bold_color "Branch aborted" 
		!cheated_gdk_color
	     else pango_markup_bold_color "Branch finished" 
		!proved_complete_gdk_color)
	    pt.open_goals_count
	    (if pt.open_goals_count > 1 then "s" else "")
      in
      pt.window#message message


(* See mli for doc *)
let process_proof_complete state proof_name =
  match !current_proof_tree with
    | None -> 
      raise (Proof_tree_error "proof-complete without current proof tree")
    | Some pt ->
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Completed other non-current proof");
      let message =
	if pt.cheated
	then pango_markup_bold_color "False proof completed"
	  !cheated_gdk_color
	else pango_markup_bold_color "Proof completed"
	!proved_complete_gdk_color
      in
      pt.window#message message;
      stop_proof_tree_last_selected pt state


(** Delete the proof tree structure with the given name from the lists
    of live and not-cloned proof tree structures. This function is
    used for {!Proof_window.delete_proof_tree_callback}.
*)
let clear_proof_tree_lists proof_name =
  let proof_tree_list_fold_fun pts pt =
    if pt.proof_name = proof_name
    then begin
      pt.window#delete_proof_window;
      pts
    end
    else pt :: pts
  in
  original_proof_trees := 
    List.fold_left proof_tree_list_fold_fun [] !original_proof_trees

let quit_proof proof_name =
  (match !current_proof_tree with 
    | None -> ()
    | Some pt ->
      if pt.proof_name = proof_name
      then begin
	current_proof_tree := None;
	emacs_callback_stop_display ();	  
      end
  );
  clear_proof_tree_lists proof_name

let _ = delete_proof_tree_callback := quit_proof


let finish_drawing () = match !current_proof_tree with
  | None -> ()
  | Some pt -> 
    if pt.sequent_area_needs_refresh then
      pt.window#refresh_sequent_area;
    if pt.need_redraw then begin
      pt.window#refresh_and_position;
      pt.window#update_ext_dialog;
    end;
    pt.sequent_area_needs_refresh <- false;
    pt.need_redraw <- false
      

(** Take the necessary actions when the configuration record changed.
    Calls the {!Proof_window.proof_window.configuration_updated}
    method on all live proof windows.
*)
let configuration_updated () =
  List.iter (fun pt -> pt.window#configuration_updated)
    !original_proof_trees;
  List.iter (fun ptw -> ptw#configuration_updated)
    !cloned_proof_windows
