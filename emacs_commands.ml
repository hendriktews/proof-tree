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


(** Generate and output emacs callback requests *)


(** Print [cmd] as emacs callback command. *)
let emacs_callback cmd =
  Printf.printf "\nemacs exec: %s\n%!" cmd

(** Print [cmd] as emacs callback command with a long data section. *)
let emacs_long_callback cmd data =
  Printf.printf "\nemacs exec: %s %d\n%s\n%!" cmd (String.length data) data

(* 
 * let emacs_long_callback cmd data =
 *   Printf.printf "\nemacs exec: %s %d\n%s%!" cmd (String.length data) 
 *     (String.sub data 0 2);
 *   Unix.sleep 1;
 *   Printf.printf "%s%!" (String.sub data 2 4);
 *   Unix.sleep 1;
 *   Printf.printf "%s\n%!" (String.sub data 4 (String.length data - 4))
 *)



(** Issue the stop-displaying emacs callback command. *)
let emacs_callback_stop_display () =
  emacs_callback "stop-displaying"


(** Send an undo command to emacs. *)
let emacs_callback_undo undo_state =
  emacs_callback (Printf.sprintf "undo %d" undo_state)


(** Send a piece of proof script to Proof General. *)
let emacs_send_proof_script script =
  emacs_long_callback "insert-proof-script" script
