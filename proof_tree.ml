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

(*****************************************************************************
 *****************************************************************************)
(** {2 Updating sequents for instantiated existential variables}

    Coq prior to 8.11 can only print sequents for the current state.
   Therefore for prooftree prior to 0.14 the Proof General
   preprocessing code kept track of the instantiation status of
   existential variables and issued [Show Goal] commands synchronously
   before the next proof command in the proof was processed.

   Coq 8.11 can print sequents for older states and I removed a lot of
   preprocessing code from Proof General for prooftree 0.14. Now only
   prooftree tracks the instantiation status of existential variables
   and requests update sequent commands from Proof General via
   [show-goal] request messages when an existential appearing in some
   sequent gets instantiated. In the first design, implemented in
   August 2019, I completely relied on the information Coq provides in
   the dependent evar line. The existential variables appearing in a
   sequent were always extracted from that line. Assume that the term
   that instantiates evar [?a] uses evar [?b]. In this first design
   update sequent commands for the instantiation of [?b] can only be
   sent out for sequents for which the update sequent command for [?a]
   has been processed. It can therefore happen that some update
   sequent commands can only be issued after some round trips to Proof
   General and Coq after the current proof has been finished. This
   conflicts with the design decision to switch off the dependend evar
   line with Proof General immediately after the proof has been
   finished. The conflict produced evar parsing errors and missing
   evar information because of the missing evar line in update sequent
   commands produced after the proof has been finished.

   I therefore revised the design on the assumption that an
   existential can only occur in a sequent because it
   {ul
   {- occurs already in the initial version of the sequent, or it}
   {- occurs in the instantiation of some existential that is known
      to occur in this sequent alredy.}
   }

   Under this assumption it is sufficient to keep the dependency tree
   of existential variables downward closed on registered sequents
   (see {!Draw_tree.existential_variable}) to maintain the following
   invariant: For all complete sequents and all known existential
   variables,
   {ul
   {- the [show goal] request message for all instantiated existentials 
      has been sent, and}
   {- all sequents are registered on all open existential variables that
      appear in them}
   }

   Here, complete sequents are sequents that have at least some
   version of sequent text. Only additionally spawned subgoals, still
   waiting for their first update sequent command, are incomplete.

   With the above invariant an update sequent command for a complete
   sequent does not trigger any [show goal] messages. These messages
   are only necessary when an existential is instantiated, when a new
   complete sequent arrives or when the first sequent text arrives for
   an incomplete sequent. Therefore, prooftree only has to delay the
   [confirm-proof-complete] message until all sequents are complete.

   However, the [show-goal] request for an incomplete sequent might be
   still on its way to Proof General when a proof is finished and the
   [proof-complete] message arrives. Therefore this module counts the
   incomplete sequents for the current proof and delays the
   [confirm-proof-complete] message until this count is zero. It is
   important to send this [confirm-proof-complete] message only after
   instantiation of existential variables, such that all [show-goal]
   messages for evar instantiation have been issued already.
 *)

(*****************************************************************************
 *****************************************************************************)
(** {2 Module Code}

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

  mutable incomplete_sequents_count : int;
  (** Counter for the number of incomplete sequents *)

  existential_hash : (string, existential_variable) Hashtbl.t;
  (** Mapping containing all existential variables in the proof tree.
      Maps internal evar names to existential records.
      Needed to establish the dependency links in instantiated
      existentials, for sequent updates on instantiation and more.
  *)

  mutable proof_complete_confirmation_pending : bool;
  (** [true] after receiving a proof complete message until
     {incomplete_sequents_count} becomes zero and the confirmation message is
     sent
   *)

  mutable need_redraw : bool;
  (** [true] if the tree display needs a redraw. Used to delay redrawing. *)

  mutable sequent_area_needs_refresh : bool;
  (** [true] if the sequent area needs to be refreshed. Used to delay
      sequent area refreshs.
  *)

  mutable undo_actions : (unit -> unit) list Int_map.t
(** {!Util.Int_map} of undo actions mapping undo states to lists of undo
    actions. Such a list is executed, if an undo to a state stricly less
    than the one associated to the list occurs. Asynchronous sequent 
    updates require adding undo actions to states long before the last one.
 *)
}
(** State record for displayed proof trees. The code maintains the
    following invariants.
    {ul 
    {- Each displayed proof tree is in precisely one the lists
    {!original_proof_trees} or inside a proof window in
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


(** Try to find a original (non-cloned) proof tree for [proof_name].
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
    incomplete_sequents_count = 0;
    existential_hash = Hashtbl.create 251;
    proof_complete_confirmation_pending = false;
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
  pt.incomplete_sequents_count <- 0;
  pt.proof_complete_confirmation_pending <- false;
  pt.need_redraw <- true;
  pt.sequent_area_needs_refresh <- true;
  pt.undo_actions <- Int_map.empty;
  pt.window#message ""


(******************************************************************************
 *****************************************************************************)
(** {2 Evar Record treatment} *)
(*****************************************************************************)

(** Propagate sequents registered for receiving updates on evar
   instanciation downward in the dependency tree of existential
   variables. This must be done on instantiating existential variables
   to ensure that all complete sequents are registered at all their
   evars such that an update sequent command can be triggered
   immediately when one of these evars is instantiated.

   Note that complex strategies might create and instatiate several
   evars at once and that these are processed in no particular order.
   It may therefore happen that evars used in the instantiation of a
   particular existental are instantiated themselves already. The
   downward propagation must therefore be continued recursively on all
   instantiated dependencies.

   Note further, that it is not necessary to issue update sequent
   commands for sequents added to already instantiated evars for the
   following reason. First, {!instantiate_existential} triggers update
   sequent commands for all existentials that got instantiated in a
   particular state. Second, Coq does not permit to instantiate an
   existential with a term containing an instantiated existential
   (this is actually an assumption here, that I validated with some
   [instanciate] commands). Therefore, if we see here that evar [?a]
   was instanciated with a term containing evar [?b] and [?b] is
   instantiated as well, then it must have been a complex strategy
   that first instanciated [?a] and later [?b]. And it appears as if
   [?a] and [?b] have been instanciated in the same state. Now, the
   sequents registed with [?a] are in general not registered with [?b]
   (especially not when [?b] is new). But all sequents registered with
   [?a] receive already an update for this state, because [?a] is
   instantiated here. Therefore we don't need to trigger another
   update for the same state for [?b].
 *)
let rec propagate_registered_sequents ex =
  List.iter
    (fun dep ->
      dep.evar_sequents <-
        Int_map.union
          (fun _ seqs1 seqs2 -> Some (seqs1 @ seqs2))
          ex.evar_sequents dep.evar_sequents)
    ex.evar_deps;
  List.iter
    (fun dep ->
      if dep.evar_status <> Uninstantiated
      then
        propagate_registered_sequents dep)
    ex.evar_deps      

(** Mark the given existential as instantiated, link it with its
   dependencies, initiate sequents updates for all sequents containing
   the existential and propagate sequents registered for the evar
   downward to the dependencies. Argument proof_name makes a round
   trip to PG, in order to identify the proof tree window when it
   comes back. Argument state is the undo state und dependency_names
   is the list of internal evar names occuring in the instantiation of
   evar ex. Argument sw_hash contains those sequents (as key, mapped
   to true) for which a sequent update request for this state state
   has already been sent (for a different evar that got also
   instantiated in this state).
*)
let instantiate_existential proof_name state ex_hash sw_hash ex dep_names =
  (* Printf.fprintf (debugc()) "inst %s at state %d\n%!"
   *   ex.evar_internal_name state;
   *)
  assert(ex.evar_deps = []);
  ex.evar_status <- Partially_instantiated;
  ex.evar_inst_state <- state;
  ex.evar_deps <- List.rev_map (Hashtbl.find ex_hash) dep_names;
  Int_map.iter
    (fun _ ->
      List.iter
        (fun s ->
          if not (Hashtbl.mem sw_hash s) then
            begin
              (* Printf.fprintf (debugc()) "request update for sequent %s\n%!"
               *   s#id;
               *)
              emacs_send_show_goal proof_name state s#id;
              Hashtbl.add sw_hash s true
            end))
    ex.evar_sequents;
  propagate_registered_sequents ex


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


(** Register new existentials in the hash of existential variables and
   filter newly instanciated existentials (without instanciating
   them).

   Returns two lists, of which the first one contains the new
   existentials as {!Draw_tree.existential_variable}. The second list
   is a list of pairs [(ex, deps)] of a newly instanciated existential
   together with the list of evars appearing in the instanciation.
   Argument [evar_info] is the (first part of the) evar information
   returned from the evar information parser. It is scanned for new
   existentials and newly instantiated ones. Note that new
   existentials can even be found in the dependencies of instantiated
   existentials, because the order inside [evar_info] may be arbitrary
   and also because some complex strategy might create and instantiate
   several existentials. Newly created existentials are registered in
   the hash of existential variables.
 *)
let create_and_filter_new_existentials ex_hash evar_info
    : (existential_variable list * (existential_variable * string list) list) =
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
  (* Process evar info, create evars as needed, return the list of new
   * evars and the list of instantiated evars, the latter without
   * instantiating them.
   *)
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
          then (ex, deps) :: accu_inst
          else accu_inst
        in
        (accu_new, accu_inst)
  in
  (* XXX sort the new evars? With a coq specific comparison function? *)
  List.fold_left process_evar_info ([], []) evar_info


(** Create and instantiate existentials and filter newly instanciated
   evars.

   Returns two lists of {!Draw_tree.existential_variable}: the new
   evars and the instantiated ones. Argument [evar_info] is the (first
   part of the) evar information returned from the evar information
   parser. It is scanned for new existentials and newly instantiated
   ones. Newly created existentials are registered in the hash of
   existential variables. Instantiated existentials are updated and
   update sequent requests are sent for affected sequents. Downward
   closure of sequents in the evar dependency tree (see
   {!Draw_tree.existential_variable}) is maintained here by
   propagating sequents down on instantiation. Argument [state] is the
   proof assistant state from which the information about existential
   variables was collected. It is needed for triggering sequent
   updates for sequents containing newly instantiated evars. Argument
   [proof_name] makes a round trip to Proof General in show goal
   commands to identify the proof tree window in the sequent update.
   *)
let create_and_instanciate_existentials proof_name state ex_hash evar_info
                    : (existential_variable list * existential_variable list) =
  let (new_existentials, inst_existentials) =
    create_and_filter_new_existentials ex_hash evar_info in
  (* A sequent might contain several evars that all get instantiated
   * at the same time. We should nevertheless send out only one update
   * sequent request.
   *)
  let requested_update_hash = Hashtbl.create 256 in
  let inst_existentials =
    List.rev_map
      (fun (ex, dep_names) ->
        instantiate_existential proof_name state ex_hash requested_update_hash
          ex dep_names;
        ex)
      inst_existentials
  in
  (new_existentials, inst_existentials)


(** Filter those evars from [current_goal_evars] that are not
   instantiated in the current state. This can simply be done by
   looking them up in the existential hash. Return the list of open
   evars as {!Draw_tree.existential_variable}. Argument
   [current_goal_evars] is the second part returned from the evar
   information parser.
 *)
let filter_open_existentials_current_state ex_hash current_goal_evars =
  List.fold_left
    (fun current_open int_name ->
      let ex = Hashtbl.find ex_hash int_name in
      match ex.evar_status with
        | Uninstantiated ->  ex :: current_open
        | Partially_instantiated
        | Fully_instantiated -> current_open)
    []
    current_goal_evars
  

(** Filter the list of open existentals in the current goal from
   argument [goal_evars] for a previous state. Argument [ex_hash] is
   the existential hash of the current state, while [evar_info] and
   [goals_evar] are from an update sequent command for a previous
   state. Therefore, existentials open in the sequent at that state
   might already be instantiated in [ex_hash] and the information
   about open existentials must be taken from [evar_info].
 *)
let filter_open_existentials_old_state ex_hash evar_info goal_evars =
  let open_evars = Hashtbl.create 57 in
  List.iter
    (function
     | Noninstantiated(int_name, _) -> Hashtbl.add open_evars int_name true
     | Instantiated _ -> ())
    evar_info;
  List.rev_map
    (Hashtbl.find ex_hash)
    (list_filter_rev (Hashtbl.mem open_evars) goal_evars)


(** Register sequent in all evars in evar list [evars], such that a
   show goal command can be scheduled when some evar from [evars] is
   instantiated. Argument [state] might be an old state, because
   [sequent] and [evars] might themselves come from an update sequent
   command. If some evar in [evars] has been instantiated since state
   [state], then the show goal command is sent here and the register
   process is done recursively on the dependencies. Argument
   [proof_name] identifies the proof-tree window and makes a round
   trip to Proof General. Argument [state] is also used for undo in
   the {!Util.Int_map} in
   {!Draw_tree.existential_variable.evar_sequents}. Take special care
   to send out update sequent requests ordered by ascending states
   (same order as during slow single stepping) and to send at most one
   such request for each state. This is achieved by collecting the
   states for which a request needs to be sent in an {!Util.Int_set}.
   *)
let register_and_update_sequent proof_name state sequent evars =
  let state_set = Int_set.empty in
  (* Evar [evar] appeared in state [state] in [sequent] and therefore
   * needs to be registered in evar. If evar is instantiated already,
   * we need an update for sequent in the state in which it got
   * instantiated. Besides, for downward closure (see
   * {!Draw_tree.existential_variable}), we need to recurse on
   * instantiated evars.
   *)
  let rec do_evar state evar state_set =
    (* Printf.fprintf (debugc()) "register seq %s in evar %s with state %d\n%!"
     *   sequent#id evar.evar_internal_name state;
     *)
    evar.evar_sequents <-
      Int_map.update state
        (function
         | None -> Some [sequent]
         | Some seqs -> Some (sequent :: seqs))
        evar.evar_sequents;
    match evar.evar_status with
      | Uninstantiated -> state_set
      | Partially_instantiated
      | Fully_instantiated ->
         let inst_state = evar.evar_inst_state in
         let state_set = Int_set.add inst_state state_set in
         (* Printf.fprintf (debugc()) "evar inst in state %d with deps %s\n%!"
          *   inst_state
          *   (String.concat " " (List.map (fun e -> e.evar_internal_name)
          *                         evar.evar_deps));
          *)
         List.fold_left
           (fun state_set dep -> do_evar inst_state dep state_set)
           state_set
           evar.evar_deps
  in
  let state_set =
    List.fold_left
      (fun state_set evar -> do_evar state evar state_set)
      state_set
      evars
  in
  Int_set.iter
    (fun state ->
      (* Printf.fprintf (debugc()) "request sequent %s for state %d\n%!"
       *   sequent#id state;
       *)
      emacs_send_show_goal proof_name state sequent#id)
    state_set


(** Delete all sequents registered after state [undo_state] in the map
   for registered sequents of evar [ex]. Used for undo in the current
   proof tree. *)
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
    less than [pa_state].
*)
let add_undo_action pt pa_state undo_fun =
  pt.undo_actions <-
    Int_map.update pa_state
      (function
       | None -> Some [undo_fun]
       | Some fs -> Some (undo_fun :: fs))
      pt.undo_actions

(** Closes the proof tree [pt], leaving the current branch open if
    there is a current branch.
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


(** Same as {!stop_proof_tree} but if there is no selected sequent,
   select the original proof obligation (the root node of the proof
   tree) as selected sequent.
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

(** Perform undo to [pa_state] inside the current proof tree [pt]. For
   this, the undo actions must be executed and some sequents must be
   unregistered at some evars. *)
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
  let (new_evars, inst_evars) =
    create_and_filter_new_existentials pt.existential_hash evar_info in
  (* Printf.fprintf (debugc())
   *   "new layer %s at state %d, inst %s, this goal open none\n%!"
   *   current_sequent_id
   *   state
   *   (String.concat " "
   *      (List.rev_map (fun e -> e.evar_internal_name) inst_evars));
   *)
  (* If this assertion breaks, something more must be done about the
   * part that is not empty any more.
   *)
  assert (new_evars = [] && inst_evars = [] && current_evar_names = []);
  let first_sw =
    pt.window#new_turnstile state current_sequent_id 
      (Some current_sequent_text) in
  Hashtbl.add pt.sequent_hash current_sequent_id first_sw;
  let first_sw = (first_sw :> proof_tree_element) in
  List.iter (emacs_send_show_goal pt.proof_name state) additional_ids;
  let other_sw =
    List.map
      (fun id ->
	let sw = pt.window#new_turnstile state id None in
	Hashtbl.add pt.sequent_hash id sw;
	(sw :> proof_tree_element))
      additional_ids
  in
                                                  (* inside create_new_layer *)
  let new_incomplete_sequents_count = List.length other_sw in
  let all_roots = first_sw :: other_sw in
  let layer = new tree_layer all_roots in
  let layer_undo_pos = pt.window#layer_stack#add_layer layer in
  let old_incomplet_sequents_count = pt.incomplete_sequents_count in
  pt.current_sequent_id <- Some current_sequent_id;
  pt.current_sequent <- Some first_sw;
  pt.open_goals_count <- new_incomplete_sequents_count + 1;
  pt.incomplete_sequents_count <-
    pt.incomplete_sequents_count + new_incomplete_sequents_count;
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
    pt.incomplete_sequents_count <- old_incomplet_sequents_count;
  in
                                                  (* inside create_new_layer *)
  (* Printf.fprintf (debugc()) "new layer incomp count %d\n%!"
   *   pt.incomplete_sequents_count; *)
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
  let (new_evars, inst_evars) =
    create_and_instanciate_existentials
      pt.proof_name state pt.existential_hash evar_info in
  let current_open_evars =
    filter_open_existentials_current_state pt.existential_hash current_evar_names
  in
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
                                      (***************** inside add_new_goal *)
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
  register_and_update_sequent
    pt.proof_name state (sw :> turnstile_interface) current_open_evars;
  let new_incomplete_sequents_count = List.length new_goals in
  let unhash_sequent_ids = current_sequent_id :: new_goal_ids_rev in
  let old_current_sequent_id = pt.current_sequent_id in
  let old_current_sequent = parent in
  let old_open_goals_count = pt.open_goals_count in
  let old_incomplet_sequents_count = pt.incomplete_sequents_count in
                                      (***************** inside add_new_goal *)
  pt.current_sequent_id <- Some current_sequent_id;
  pt.current_sequent <- Some sw;
  pt.open_goals_count <- pt.open_goals_count + new_incomplete_sequents_count;
  pt.incomplete_sequents_count <-
    pt.incomplete_sequents_count + new_incomplete_sequents_count;
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
                                      (***************** inside add_new_goal *)
  (* Printf.fprintf (debugc()) "new goal %s incomp count %d\n%!"
   *   sw#id pt.incomplete_sequents_count; *)
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
    pt.incomplete_sequents_count <- old_incomplet_sequents_count;
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
                  _current_evar_names =
  (* Ignore the current current_evar_names here. They might actually
   * contain something if this function is called as part of
   * finish_branch_and_switch_to.
   *)
  let (new_evars, inst_evars) =
    create_and_instanciate_existentials
      pt.proof_name state pt.existential_hash evar_info
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
  (* Printf.fprintf (debugc()) "finish branch incomp count %d\n%!"
   *   pt.incomplete_sequents_count; *)
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


(** Udate the sequent text for some sequent and set an appropriate
   undo action. For incomplete sequents (i.e., additionally spawned
   subgoals that now receive their initial sequent text) the dependent
   evars info comming from the evar parser (arguments [evar_info] and
   [current_evar_names]) is processed to register this sequent for
   sequent updates on all evars that were open in state [state] where
   sequent [sw] was created. All necessary sequent updates are
   requested immediately. If necessary and if the counter of
   incomplete sequences reaches zero, the [confirm-proof-complete]
   message is sent. Argument [state] is the potentially old state for
   which the sequent update was requested.
 *)
let update_sequent_element pt state sw sequent_text evar_info
      current_evar_names =
  (* Printf.fprintf (debugc()) "!! update %s sequent %s incomp count %d\n%!"
   *   (if sw#is_incomplete then "incomplete" else "complete")
   *   sw#id pt.incomplete_sequents_count; *)
  if sw#is_incomplete then begin
      (* This is the first update sequent command arriving for an
       * additionally spawned subgoal. In this case we must register
       * this sequent in all its existentials.
       *)
      let current_open_evars =
        filter_open_existentials_old_state
          pt.existential_hash evar_info current_evar_names in
      (* Printf.fprintf (debugc()) "update %s for %d, this goal open %s\n%!"
       *   sw#id state
       *   (String.concat " "
       *      (List.rev_map (fun e -> e.evar_internal_name) current_open_evars));
       *)
      register_and_update_sequent pt.proof_name state
        (sw :> turnstile_interface) current_open_evars;
      pt.incomplete_sequents_count <- pt.incomplete_sequents_count - 1;
      if pt.incomplete_sequents_count = 0
         && pt.proof_complete_confirmation_pending
      then
        emacs_confirm_proof_complete pt.proof_name;
    end
  else
    (* This is an update sequent command caused by some instantiation
     * of some existential in the sequent. This sequent is already
     * registered in all existentials.
     *)
    ();
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
  (* update sequent commands should arrive now before the current proof is
   * closed, therefore the code path for looking up an arbitrarily old
   * proof tree should never be taken any more. But it doesn't really
   * hurt, so keep it.
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
       (* XXX A quick undo might overtake show goal requests. The
        * upate sequent command might arrive here, after the undo
        * deleted the sequent already.
        *)
       (* if sequent_id = "552" then
        *   List.iter
        *     (fun sw ->
        *       Printf.fprintf (debugc()) "!! sw#id %s -> [%s]\n%!"
        *         sw#id
        *         (String.concat " | " (sw#sequent_text_history)))
        *     (Hashtbl.find_all pt.sequent_hash sequent_id); 
        *)
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
      (* XXX show goal commands might come later, therefore maybe stop
       * the current proof tree only when they will have all arrived.
       *)
      stop_proof_tree_last_selected pt state;
      if pt.incomplete_sequents_count = 0 then
        emacs_confirm_proof_complete pt.proof_name
      else
        pt.proof_complete_confirmation_pending <- true


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

(* see mli for doc *)
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


(* see mli for doc *)
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
