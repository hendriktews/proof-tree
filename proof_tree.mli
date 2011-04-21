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
 * $Id: proof_tree.mli,v 1.4 2011/04/21 13:28:10 tews Exp $
 *)


(** Internal representation of proof trees with undo info *)


(** Process the new current goal, updating the display accordingly.

    @param state state for undo
    @param proof_name name of the proof
    @param proof_command command issued to the prover
    @param current_sequent_id ID of current sequent
    @param current_sequent_text the current sequent itself
    @param additional_ids ID's of the additionally open goals
*)
val process_current_goals :
  int -> string -> string -> string -> string -> string list -> unit


(** Update the sequent to show the new sequent text.

    @param state state for undo
    @param proof_name name of proof
    @param sequent_id ID of sequent to update
    @param sequent_text new sequent text
*)
val update_sequent : int -> string -> string -> string -> unit


(** Switch to a different open goal.

    @param state state for undo
    @param proof_name name of proof
    @param new_current_id id of new current goal
*)
val switch_to : int -> string -> string -> unit


(** Finish the current proof.

    @param state state for undo
    @param proof_name name of the proof
    @param proof_command last command
*)
val process_proof_complete : int -> string -> string -> unit

(** Undo all changes up to and including state [state]. Proof trees started 
    later than [state] will be deleted. Those finished earlier than [state]
    remain untouched. All proof trees will be identical to the point in time
    before the first action with state [state] has been processed.
*)
val undo : int -> unit


(** For efficiency in proof replay the proof tree display is not redrawn
    after every change. Changes are only recorded in the internal data 
    structures. This function cases a redisplay of the current proof tree.
*)
val finish_drawing : unit -> unit
