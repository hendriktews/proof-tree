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
 * $Id: proof_tree.mli,v 1.1 2011/04/13 10:47:08 tews Exp $
 *)


(** Internal representation of proof trees with undo info *)


(** Starts a new proof tree for the proof [proof_name] and
    (re-)displays its window. The proof tree starts in state [state],
    that is, undos to or beyone [state] will delete the tree and its
    window.

    @param state
    @param proof_name name of the proof
*)
val start : int -> string -> unit


(** Add or update a sequent.

    @param state for undo
    @sequent_id the id of the sequent to add or update
    @sequent_text the UTF-8 encoded text of the sequent
*)
val add_or_update_sequent : int -> string -> string -> unit


(** Add a proof command.

    @param state for undo
    @param text the UTF-8 encoded proof command
*)
val add_proof_command : int -> string -> unit


(** Finish the current branch.

    @param state for undo
*)
val finish_branch : int -> unit


(** Switch to a different sequent and mark it current.

    @param state for undo
    @param sequent_id id of the sequent
*)
val switch_to_sequent : int -> string -> unit


(** Finish the current proof.

    @param state for undo
*)
val finish_proof : int -> unit


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
