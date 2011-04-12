open Util
open Configuration
open Draw_tree
open Proof_window

exception Proof_tree_error of string

type proof_tree_state = 
  | Start
  | Initial_sequent of proof_tree_element
  | Current_command of proof_tree_element
  | Current_sequent of proof_tree_element * proof_tree_element
  | Branch_finished

type proof_tree = {
  window : proof_window;
  proof_name : string;
  pa_start_state : int;
  mutable pa_end_state : int;		(* -1 if not finished yet *)
  mutable state : proof_tree_state;
  sequent_hash : (string, turnstile) Hashtbl.t;
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
    (match pt.state with
      | Start 
      | Branch_finished -> ()
      | Initial_sequent pte
      | Current_command pte
      | Current_sequent(_, pte) ->
	pte#mark_current;
	pt.window#set_current_node pte;
    );
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

let start pa_state proof_name =
  (match !current_proof_tree with
    | Some pt -> stop_proof_tree pt pa_state
    | None -> ());
  let new_pt = {
    window = make_proof_window proof_name !geometry_string;
    proof_name = proof_name;
    pa_start_state = pa_state;
    pa_end_state = -1;
    state = Start;
    sequent_hash = Hashtbl.create 503;
    need_redraw = true;
    undo_actions = [];
  }
  in
  assert(List.for_all 
	   (fun pt -> pt.proof_name <> proof_name) !all_proof_trees);
  current_proof_tree := Some new_pt;
  all_proof_trees := new_pt :: !all_proof_trees


let finish_proof pa_state =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Finish proof without current proof tree")
    | Some pt -> 
      (match pt.state with
	| Start
	| Branch_finished -> ()
	| Initial_sequent pte
	| Current_command pte
	| Current_sequent(_, pte) -> 
	  let old_state = pt.state in
	  let undo () =
	    pte#unmark_proved;
	    pt.state <- old_state
	  in
	  pte#mark_proved;
	  add_undo_action pt pa_state undo
      );
      stop_proof_tree pt pa_state


let add_sequent pt pa_state sequent_id sequent_text =
  (match pt.state with
    | Initial_sequent _ -> raise (Proof_tree_error "Multiple initial sequents")
    | Branch_finished -> raise (Proof_tree_error "Missing switch-to command")

    | Start
    | Current_command _
    | Current_sequent _ -> ());
  let old_state = pt.state in
  let sw = pt.window#new_turnstile sequent_id sequent_text in
  Hashtbl.add pt.sequent_hash sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  let undo () =
    remove_child sw;
    Hashtbl.remove pt.sequent_hash sequent_id;
    pt.state <- old_state
  in
  add_undo_action pt pa_state undo;
  pt.state <- (match pt.state with
    | Initial_sequent _ 
    | Branch_finished -> assert false;

    | Start -> 
      pt.window#set_root sw;
      sw#mark_current;
      pt.window#set_current_node sw;
      add_undo_action pt pa_state 
	(fun () ->
	  pt.window#clear_root;
	  sw#unmark_current
	);
      Initial_sequent (sw :> proof_tree_element)

    | Current_command pc -> 
      add_child pc sw; 
      sw#mark_current;
      pt.window#set_current_node sw;
      add_undo_action pt pa_state (fun () -> sw#unmark_current);
      Current_sequent(pc, sw)

    | Current_sequent(pc, csw) as state -> 
      pt.window#set_current_node csw;
      add_child pc sw; 
      state
  );
  pt.need_redraw <- true

let update_sequent pt pa_state sw sequent_text =
  let old_text = sw#content
  in
  sw#update_sequent sequent_text;
  pt.need_redraw <- true;
  add_undo_action pt pa_state (fun () -> sw#update_sequent old_text)

let add_or_update_sequent pa_state sequent_id sequent_text =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Add sequent without current proof tree")
    | Some pt ->
      try
	let sw = Hashtbl.find pt.sequent_hash sequent_id in
	update_sequent pt pa_state sw sequent_text
      with
	| Not_found -> add_sequent pt pa_state sequent_id sequent_text
  

let add_proof_command pa_state proof_command =
  let proof_command = chop_final_newlines proof_command in
  match !current_proof_tree with
    | None -> 
      raise (Proof_tree_error "Add proof command without current proof tree")
    | Some pt -> 
      match pt.state with
	| Start -> raise (Proof_tree_error "Add proof command with no sequent")
	| Current_command _ -> raise (Proof_tree_error "Second proof command")
	| Branch_finished -> 
	  raise (Proof_tree_error "Missing switch-to command")

	| Initial_sequent turn
	| Current_sequent(_, turn) ->
	  let old_state = pt.state in
	  let pc = pt.window#new_proof_command proof_command in
	  let undo () =
	    pc#unmark_current;
	    pt.state <- old_state;
	    remove_child pc
	  in
	  add_child turn pc;
	  pc#mark_current;
	  pt.window#set_current_node pc;
	  pt.state <- Current_command pc;
	  add_undo_action pt pa_state undo;
	  pt.need_redraw <- true


let switch_to_sequent_switch pt pa_state sequent_id =
  let sw = 
    try
      Hashtbl.find pt.sequent_hash sequent_id 
    with
      | Not_found -> raise (Proof_tree_error "Unknown switch to sequent")
  in
  let old_state = pt.state in
  let sw = (sw :> proof_tree_element) in
  (match pt.state with
    | Start
    | Current_command _
    | Initial_sequent _ -> assert false

    | Current_sequent(_, csw) -> 
      csw#unmark_current

    | Branch_finished -> ()
  );
  let pc_before_sw = match sw#parent with
    | Some p -> p
    | None -> raise (Proof_tree_error
		       "Internal error: sequent without parent")
  in
  let undo () =
    sw#unmark_current;
    pt.state <- old_state
  in
  pt.state <- Current_sequent(pc_before_sw, sw);
  sw#mark_current;
  pt.window#set_current_node sw;
  add_undo_action pt pa_state undo;
  pt.need_redraw <- true



let switch_to_sequent pa_state sequent_id =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Switch to without current proof tree")
    | Some pt ->
      match pt.state with
	| Start -> raise (Proof_tree_error "Switch to with empty proof tree")
	| Current_command _ -> 
	  raise (Proof_tree_error "Switch to without current sequent")

	| Initial_sequent csw ->
	  if csw#id = sequent_id 
	  then ()
	  else raise (Proof_tree_error "Switch away from only sequent")

	| Current_sequent(_, csw) ->
	  if csw#id = sequent_id 
	  then ()
	  else switch_to_sequent_switch pt pa_state sequent_id

	| Branch_finished -> switch_to_sequent_switch pt pa_state sequent_id


let finish_branch pa_state =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error 
		       "Finish branch without current proof tree")
    | Some pt ->
      match pt.state with
	| Start -> raise (Proof_tree_error
			    "Finish branch with empty proof tree")
	| Branch_finished -> raise (Proof_tree_error 
				      "Finish branch without current sequent")
	| Current_command pte
	| Initial_sequent pte 
	| Current_sequent(_, pte) ->
	  let old_state = pt.state in
	  let undo () =
	    pte#unmark_proved;
	    pt.state <- old_state
	  in
	  pte#mark_proved;
	  pt.state <- Branch_finished;
	  add_undo_action pt pa_state undo;
	  pt.need_redraw <- true


let finish_drawing () = match !current_proof_tree with
  | None -> ()
  | Some pt -> 
    if pt.need_redraw then 
      pt.window#refresh_and_position;
      pt.need_redraw <- false
	

