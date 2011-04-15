(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 Hendrik Tews
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the
 * parent directories for more details.
 * 
 * $Id: proof_tree.ml,v 1.7 2011/04/15 19:32:24 tews Exp $
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


let all_proof_trees = ref []

let current_proof_tree = ref None


let stop_proof_tree pt pa_state = 
  (* pt.undo_actions??? *)
  pt.pa_end_state <- pa_state;
  pt.window#disconnect_proof;
  pt.window#refresh_and_position;
  pt.need_redraw <- false;
  current_proof_tree := None


let delete_proof_tree proof_name =
  (match !current_proof_tree with 
    | None -> ()
    | Some pt ->
      if pt.proof_name = proof_name
      then current_proof_tree := None
  );
  all_proof_trees := 
    List.filter (fun pt -> pt.proof_name <> proof_name) !all_proof_trees
	

type proof_tree_undo_result =
  | PT_undo_delete
  | PT_undo_current
  | PT_undo_keep


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
    pt.window#delete_proof_window;
    PT_undo_delete
  end 
  else if pt.pa_end_state >= 0 && pa_state > pt.pa_end_state 
  then PT_undo_keep
  else begin
    pt.pa_end_state <- -1;
    pt.undo_actions <- fire_undo_actions pa_state pt.undo_actions;
    pt.current_sequent#mark_current;
    pt.window#set_current_node pt.current_sequent;
    pt.need_redraw <- true;
    PT_undo_current
  end

let undo pa_state =
  current_proof_tree := None;
  all_proof_trees :=
    List.fold_left
    (fun pts pt -> match undo_tree pt pa_state with
      | PT_undo_delete -> pts
      | PT_undo_current -> 
	current_proof_tree := Some pt;
	pt :: pts
      | PT_undo_keep -> pt :: pts)
    [] !all_proof_trees



let start_new_proof state proof_name current_sequent_id current_sequent_text =
  assert(List.for_all 
	   (fun pt -> pt.proof_name <> proof_name) !all_proof_trees);
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
  pt.window#set_root sw;
  sw#mark_current;
  pt.window#set_current_node sw;
  pt.need_redraw <- true;
  current_proof_tree := Some pt;
  all_proof_trees := pt :: !all_proof_trees


let add_new_goal pt state proof_command current_sequent_id 
    current_sequent_text additional_ids =
  let pc = pt.window#new_proof_command proof_command in
  set_children pt.current_sequent [pc];
  let sw = pt.window#new_turnstile current_sequent_id current_sequent_text in
  Hashtbl.add pt.sequent_hash current_sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  assert(list_set_subset pt.other_open_goals additional_ids);
  let new_goal_ids_rev =
    if pt.other_open_goals = additional_ids 
    then []
    else list_set_diff_rev additional_ids pt.other_open_goals
  in
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
  set_children pc (sw :: new_goals);
  sw#mark_current;
  pt.window#set_current_node sw;
  let old_current_sequent_id = pt.current_sequent_id in
  let old_current_sequent = pt.current_sequent in
  let old_other_open_goals = pt.other_open_goals in
  pt.current_sequent_id <- current_sequent_id;
  pt.current_sequent <- sw;
  pt.other_open_goals <- additional_ids;
  let undo () =
    clear_children old_current_sequent;
    old_current_sequent#mark_current;
    List.iter (fun id -> Hashtbl.remove pt.sequent_hash id) new_goal_ids_rev;
    pt.current_sequent_id <- old_current_sequent_id;
    pt.current_sequent <- old_current_sequent;
    pt.other_open_goals <- old_other_open_goals;
  in
  add_undo_action pt state undo;
  pt.need_redraw <- true


let finish_branch pt state proof_command =
  let pc = pt.window#new_proof_command proof_command in
  set_children pt.current_sequent [pc];
  pc#mark_proved;
  let old_current_sequent = pt.current_sequent in
  let undo () =
    clear_children old_current_sequent;
    old_current_sequent#unmark_proved;
  in
  add_undo_action pt state undo;
  pt.need_redraw <- true


let finish_branch_and_switch_to pt state proof_command current_sequent_id 
    additional_ids =
  assert(List.mem current_sequent_id pt.other_open_goals);
  assert(not (List.mem current_sequent_id additional_ids));
  assert(list_set_subset additional_ids pt.other_open_goals);
  finish_branch pt state proof_command;
  let new_current_sequent = Hashtbl.find pt.sequent_hash current_sequent_id in
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
  pt.current_sequent_id <- current_sequent_id;
  pt.current_sequent <- new_current_sequent;
  pt.other_open_goals <- additional_ids;
  add_undo_action pt state undo;
  pt.need_redraw <- true

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


(* XXX possibly update sequent window *)
let update_sequent_element pt state sw sequent_text =
  let old_sequent_text = sw#content in
  sw#update_sequent sequent_text;
  add_undo_action pt state (fun () -> sw#update_sequent old_sequent_text)  


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

let process_proof_complete state proof_name proof_command =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Finish proof without current proof tree")
    | Some pt -> 
      if pt.proof_name <> proof_name
      then raise (Proof_tree_error "Finish other non-current proof");
      finish_branch pt state proof_command;
      stop_proof_tree pt state




let finish_drawing () = match !current_proof_tree with
  | None -> ()
  | Some pt -> 
    if pt.need_redraw then 
      pt.window#refresh_and_position;
      pt.need_redraw <- false
	

