(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 Hendrik Tews
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
 * 
 * $Id: proof_tree.ml,v 1.13 2011/05/30 07:17:43 tews Exp $
 *)


(** Internal representation of proof trees with undo info *)


open Util
open Configuration
open Draw_tree
open Proof_window

exception Proof_tree_error of string

type proof_tree = {
  window : proof_window;
  proof_name : string;
  pa_start_state : int;
  mutable pa_end_state : int;		(* -1 if not finished yet *)
  sequent_hash : (string, turnstile) Hashtbl.t;
  mutable current_sequent_id : string;
  mutable current_sequent : proof_tree_element;
  mutable other_open_goals : string list;
  mutable need_redraw : bool;
  mutable undo_actions : (int * (unit -> unit) list) list;
}


let add_undo_action pt pa_state undo_fun =
  match pt.undo_actions with
    | [] -> pt.undo_actions <- [(pa_state, [undo_fun])]
    | (prev_pa_state, prev_undos) :: undo_tail ->
      assert(pa_state >= prev_pa_state);
      if pa_state = prev_pa_state 
      then
	pt.undo_actions <- (prev_pa_state, undo_fun :: prev_undos) :: undo_tail
      else
	pt.undo_actions <- (pa_state, [undo_fun]) :: pt.undo_actions


(** Contains all proof trees that should be affected by undo
    operations. Cloned proof trees and proof trees that survived a
    bulk undo are not in this list.
*)
let all_proof_trees_for_undo = ref []

(** Contains proof trees that survived a bulk undo. Like cloned proof
    trees these trees do not take part in undo actions. Unlike cloned
    proof trees these proof trees should be reused, when the user
    eventually starts on the proof again.
*)
let undo_surviver_trees = ref []


let current_proof_tree = ref None


let stop_proof_tree pt pa_state = 
  (* Keep undo actions, never know if the prover supports jumping in
   * to the middle of a proof.
   *)
  pt.pa_end_state <- pa_state;
  pt.window#disconnect_proof;
  pt.window#select_node pt.current_sequent;
  pt.window#refresh_and_position;
  pt.need_redraw <- false;
  current_proof_tree := None


(** Result values for [undo_tree] that tell the calling function
    [undo] what to do with the argument proof tree.
*)
type proof_tree_undo_result =
  | PT_undo_delete    (** Argument proof tree should be deleted  *)
  | PT_undo_current   (** Argument proof tree should be kept as current *)
  | PT_undo_keep      (** Argument proof tree should be kept non-current *)
  | PT_undo_surviver  (** Argument proof tree should be kept as surviver *)


let rec fire_undo_actions undo_state = function
  | [] -> []
  | ((state, undos) :: undo_rest) as undo_list ->
    if state >= undo_state 
    then begin
      List.iter (fun f -> f()) undos;
      fire_undo_actions undo_state undo_rest
    end else
      undo_list

let undo_tree pt pa_state =
  if pa_state <= pt.pa_start_state
  then begin
    if pt.window#survive_undo_before_start 
    then begin
      stop_proof_tree pt (-1);
      PT_undo_surviver
    end
    else begin
      pt.window#delete_proof_window;
      PT_undo_delete
    end
  end 
  else if pt.pa_end_state >= 0 && pa_state > pt.pa_end_state 
  then PT_undo_keep
  else begin
    pt.pa_end_state <- -1;
    pt.undo_actions <- fire_undo_actions pa_state pt.undo_actions;
    pt.current_sequent#mark_current;
    pt.window#set_current_node pt.current_sequent;
    pt.window#refresh_sequent_area;
    pt.need_redraw <- true;
    PT_undo_current
  end

let undo pa_state =
  let new_current = ref None in
  all_proof_trees_for_undo :=
    List.fold_left
    (fun pts pt -> match undo_tree pt pa_state with
      | PT_undo_delete -> pts
      | PT_undo_current -> 
	new_current := Some pt;
	pt :: pts
      | PT_undo_keep -> pt :: pts
      | PT_undo_surviver -> 
	undo_surviver_trees := pt :: !undo_surviver_trees;
	pts
    )
    [] !all_proof_trees_for_undo;
  current_proof_tree := !new_current


let get_surviver proof_name =
  let rec doit res = function
    | [] -> None
    | pt :: pts ->
      if pt.proof_name = proof_name
      then begin
	undo_surviver_trees := List.rev_append res pts;
	Some pt
      end 
      else doit (pt::res) pts
  in
  doit [] !undo_surviver_trees

let create_new_proof_tree proof_name state 
                                     current_sequent_id current_sequent_text =
  let pt_win = make_proof_window proof_name !geometry_string in
  let sw = pt_win#new_turnstile current_sequent_id current_sequent_text in
  let hash = Hashtbl.create 503 in
  Hashtbl.add hash current_sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  let pt = {
    window = pt_win;
    proof_name = proof_name;
    pa_start_state = state;
    pa_end_state = -1;
    sequent_hash = hash;
    current_sequent_id = current_sequent_id;
    current_sequent = sw;
    other_open_goals = [];
    need_redraw = true;
    undo_actions = [];
  } in
  pt_win#set_root sw;
  pt

let reuse_surviver pt state current_sequent_id current_sequent_text =
  let pt_win = pt.window in
  let proof_name = pt.proof_name in
  let hash = pt.sequent_hash in
  let sw = pt_win#new_turnstile current_sequent_id current_sequent_text in
  pt_win#clear_selected_node;
  Hashtbl.clear hash;
  Hashtbl.add pt.sequent_hash current_sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  let pt = {
    window = pt_win;
    proof_name = proof_name;
    pa_start_state = state;
    pa_end_state = -1;
    sequent_hash = hash;
    current_sequent_id = current_sequent_id;
    current_sequent = sw;
    other_open_goals = [];
    need_redraw = true;
    undo_actions = [];
  } in
  pt_win#set_root sw;
  pt

let start_new_proof state proof_name current_sequent_id current_sequent_text =
  assert(List.for_all 
	   (fun pt -> pt.proof_name <> proof_name) !all_proof_trees_for_undo);
  let pt =
    match get_surviver proof_name with
      | None -> 
	create_new_proof_tree proof_name state 
	  current_sequent_id current_sequent_text
      | Some pt -> 
	reuse_surviver pt state current_sequent_id current_sequent_text
  in
  pt.current_sequent#mark_current;
  pt.window#set_current_node pt.current_sequent;
  pt.need_redraw <- true;
  current_proof_tree := Some pt;
  all_proof_trees_for_undo := pt :: !all_proof_trees_for_undo


let add_new_goal pt state proof_command current_sequent_id 
    current_sequent_text additional_ids =
  let pc = pt.window#new_proof_command proof_command in
  let pc = (pc :> proof_tree_element) in
  set_children pt.current_sequent [pc];
  let sw = pt.window#new_turnstile current_sequent_id current_sequent_text in
  Hashtbl.add pt.sequent_hash current_sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  (* It is tempting to assert
   * 
   *     assert(list_set_subset pt.other_open_goals additional_ids);
   * 
   * However, in Coq the Focus command temporarily narrows the display of
   * the additionally open subgoals.
   *)
  let new_goal_ids_rev = list_set_diff_rev additional_ids pt.other_open_goals in
  assert(List.for_all 
	   (fun id -> not (Hashtbl.mem pt.sequent_hash id)) new_goal_ids_rev);
  let new_goals =
    List.fold_left
      (fun res id ->
	let sw = pt.window#new_turnstile id "waiting for sequent text" in
	Hashtbl.add pt.sequent_hash id sw;
	let sw = (sw :> proof_tree_element) in
	sw :: res)
      [] new_goal_ids_rev
  in
  let all_subgoals = sw :: new_goals in
  set_children pc all_subgoals;
  sw#mark_current;
  pt.window#set_current_node sw;
  let unhash_sequent_ids = current_sequent_id :: new_goal_ids_rev in
  let old_current_sequent_id = pt.current_sequent_id in
  let old_current_sequent = pt.current_sequent in
  let old_other_open_goals = pt.other_open_goals in
  pt.current_sequent_id <- current_sequent_id;
  pt.current_sequent <- sw;
  pt.other_open_goals <- 
    list_set_union_disjoint new_goal_ids_rev pt.other_open_goals;
  let undo () =
    clear_children old_current_sequent;
    old_current_sequent#mark_current;
    List.iter (fun id -> Hashtbl.remove pt.sequent_hash id) unhash_sequent_ids;
    List.iter 
      (fun sw -> if sw#is_selected then pt.window#clear_selected_node)
      all_subgoals;
    if pc#is_selected then pt.window#clear_selected_node;
    pt.current_sequent_id <- old_current_sequent_id;
    pt.current_sequent <- old_current_sequent;
    pt.other_open_goals <- old_other_open_goals;
  in
  add_undo_action pt state undo;
  pt.need_redraw <- true


let finish_branch pt state proof_command =
  let pc = pt.window#new_proof_command proof_command in
  let pc = (pc :> proof_tree_element) in
  set_children pt.current_sequent [pc];
  pc#mark_proved;
  let old_current_sequent = pt.current_sequent in
  let undo () =
    clear_children old_current_sequent;
    old_current_sequent#unmark_proved;
  in
  add_undo_action pt state undo;
  pt.need_redraw <- true

let internal_switch_to pt state old_open_sequent_id new_current_sequent_id =
  assert(match old_open_sequent_id with 
    | None -> true
    | Some id -> not (List.mem id pt.other_open_goals));
  (* The user might switch to the current sequent *)
  assert(new_current_sequent_id = pt.current_sequent_id ||
      List.mem new_current_sequent_id pt.other_open_goals);
  let new_current_sequent = 
    Hashtbl.find pt.sequent_hash new_current_sequent_id 
  in
  let new_current_sequent = (new_current_sequent :> proof_tree_element) in
  new_current_sequent#mark_current;
  pt.window#set_current_node new_current_sequent;
  let old_current_sequent_id = pt.current_sequent_id in
  let old_current_sequent = pt.current_sequent in
  let old_other_open_goals = pt.other_open_goals in
  let undo () =
    new_current_sequent#unmark_current;
    pt.current_sequent_id <- old_current_sequent_id;
    pt.current_sequent <- old_current_sequent;
    pt.other_open_goals <- old_other_open_goals;
  in
  pt.current_sequent_id <- new_current_sequent_id;
  pt.current_sequent <- new_current_sequent;
  let all_open_goals = 
    (match old_open_sequent_id with
      | None -> pt.other_open_goals
      | Some id -> list_set_add_nonpresent_element id pt.other_open_goals
    )
  in
  pt.other_open_goals <- 
    list_set_remove_element new_current_sequent_id all_open_goals;
  add_undo_action pt state undo;
  pt.need_redraw <- true


let finish_branch_and_switch_to pt state proof_command current_sequent_id 
    additional_ids =
  assert(not (List.mem current_sequent_id additional_ids));
  assert(list_set_subset additional_ids pt.other_open_goals);
  finish_branch pt state proof_command;
  internal_switch_to pt state None current_sequent_id

let process_current_goals state proof_name proof_command
    current_sequent_id current_sequent_text additional_ids =
  (match !current_proof_tree with
    | Some pt -> 
      if pt.proof_name <> proof_name 
      then stop_proof_tree pt state
    | None -> ());
  match !current_proof_tree with
    | None -> 
      assert(additional_ids = []);
      start_new_proof state proof_name current_sequent_id current_sequent_text
    | Some pt ->
      if pt.current_sequent_id <> current_sequent_id &&
	Hashtbl.mem pt.sequent_hash current_sequent_id
      then
	finish_branch_and_switch_to pt state proof_command 
	  current_sequent_id additional_ids
      else
	add_new_goal pt state proof_command current_sequent_id 
	  current_sequent_text additional_ids


let change_sequent_text proof_window sequent text () =
  sequent#update_sequent text;
  if sequent#is_selected then proof_window#refresh_sequent_area

let update_sequent_element pt state sw sequent_text =
  let old_sequent_text = sw#content in
  change_sequent_text pt.window sw sequent_text ();
  add_undo_action pt state (change_sequent_text pt.window sw old_sequent_text)  

let update_sequent state proof_name sequent_id sequent_text =
  match !current_proof_tree with
    | None ->
      raise (Proof_tree_error "Update sequent without current proof tree")
    | Some pt ->
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Update sequent on other non-current proof");
      try
	update_sequent_element pt state 
	  (Hashtbl.find pt.sequent_hash sequent_id) sequent_text
      with
	| Not_found ->
	  raise (Proof_tree_error "Update unknown sequent")


let switch_to state proof_name new_current_sequent_id =
  match !current_proof_tree with
    | None ->
      raise (Proof_tree_error "Switch to sequent without current proof tree")
    | Some pt ->
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Switch to sequent on other proof");
      pt.current_sequent#unmark_current;
      internal_switch_to pt state
	(Some pt.current_sequent_id) new_current_sequent_id

let process_proof_complete state proof_name proof_command =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Finish proof without current proof tree")
    | Some pt -> 
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Finish other non-current proof");
      finish_branch pt state proof_command;
      stop_proof_tree pt state


let quit_proof proof_name =
  (match !current_proof_tree with 
    | None -> ()
    | Some pt ->
      if pt.proof_name = proof_name
      then current_proof_tree := None
  );
  all_proof_trees_for_undo := 
    List.fold_left
    (fun pts pt -> 
      if pt.proof_name = proof_name
      then begin
	pt.window#delete_proof_window;
	pts
      end
      else pt :: pts)
    [] !all_proof_trees_for_undo


let finish_drawing () = match !current_proof_tree with
  | None -> ()
  | Some pt -> 
    if pt.need_redraw then 
      pt.window#refresh_and_position;
      pt.need_redraw <- false
	

