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
  mutable state : proof_tree_state;
  sequent_hash : (string, turnstile) Hashtbl.t;
  mutable need_redraw : bool;
}

let current_proof_tree = ref None


let stop_proof_tree pt = 
  pt.window#disconnect_proof;
  pt.window#refresh_and_position;
  pt.need_redraw <- false;
  current_proof_tree := None


let start name =
  (match !current_proof_tree with
    | Some pt -> stop_proof_tree pt
    | None -> ());
  current_proof_tree := Some {
    window = make_proof_window name !geometry_string;
    state = Start;
    sequent_hash = Hashtbl.create 503;
    need_redraw = true;
  }

let finish_proof () =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Finish proof without current proof tree")
    | Some pt -> 
      (match pt.state with
	| Start
	| Branch_finished -> ()
	| Initial_sequent pte
	| Current_command pte
	| Current_sequent(_, pte) -> pte#mark_proved
      );
      stop_proof_tree pt;
      pt.need_redraw <- true


let add_sequent pt sequent_id sequent_text =
  (match pt.state with
    | Initial_sequent _ -> raise (Proof_tree_error "Multiple initial sequents")
    | Branch_finished -> raise (Proof_tree_error "Missing switch-to command")

    | Start
    | Current_command _
    | Current_sequent _ -> ());
  let sw = pt.window#new_turnstile sequent_id sequent_text in
  Hashtbl.add pt.sequent_hash sequent_id sw;
  let sw = (sw :> proof_tree_element) in
  pt.state <- (match pt.state with
    | Initial_sequent _ 
    | Branch_finished -> assert false;

    | Start -> 
      sw#mark_current;
      pt.window#set_root sw;
      pt.window#set_current_node sw;
      Initial_sequent (sw :> proof_tree_element)

    | Current_command pc -> 
      add_child pc sw; 
      sw#mark_current;
      pt.window#set_current_node sw;
      Current_sequent(pc, sw)

    | Current_sequent(pc, csw) as state -> 
      pt.window#set_current_node csw;
      add_child pc sw; 
      state
  );
  pt.need_redraw <- true


let add_or_update_sequent sequent_id sequent_text =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Add sequent without current proof tree")
    | Some pt ->
      try
	let sw = Hashtbl.find pt.sequent_hash sequent_id in
	sw#update_sequent sequent_text
      with
	| Not_found -> add_sequent pt sequent_id sequent_text
  

let add_proof_command proof_command =
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
	  let pc = pt.window#new_proof_command proof_command in
	  add_child turn pc;
	  pc#mark_current;
	  pt.window#set_current_node pc;
	  pt.state <- Current_command pc;
	  pt.need_redraw <- true


let switch_to_sequent sequent_id =
  match !current_proof_tree with
    | None -> raise (Proof_tree_error "Switch to without current proof tree")
    | Some pt ->
      let sw = 
	try
	  Hashtbl.find pt.sequent_hash sequent_id 
	with
	  | Not_found -> raise (Proof_tree_error "Unknown switch to sequent")
      in
      let sw = (sw :> proof_tree_element) in
      (match pt.state with
	| Start -> raise (Proof_tree_error "Switch to with empty proof tree")
	| Current_command _ -> 
	  raise (Proof_tree_error "Switch to without current sequent")
	| Initial_sequent csw ->
	  raise (Proof_tree_error "Switch to with initial sequent")

	| Current_sequent(_, csw) -> 
	  csw#unmark_current

	| Branch_finished -> ()
      );
      let pc_before_sw = match sw#parent with
	| Some p -> p
	| None -> raise (Proof_tree_error
			   "Internal error: sequent without parent")
      in
      pt.state <- Current_sequent(pc_before_sw, sw);
      sw#mark_current;
      pt.window#set_current_node sw;
      pt.need_redraw <- true


let finish_branch () =
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
	  pte#mark_proved;
	  pt.state <- Branch_finished;
	  pt.need_redraw <- true


let finish_drawing () = match !current_proof_tree with
  | None -> ()
  | Some pt -> 
    if pt.need_redraw then 
      pt.window#refresh_and_position;
      pt.need_redraw <- false
	

