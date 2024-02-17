(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2024 Hendrik Tews
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

(* needed only for debug printing
 * open Util
 *)

(* sends 14 chars + cmd *)
(* XXX assert that the whole command, including both newlines, are
   less than PIPE_BUF bytes, get the latter from configure and getconf
   -a, getconf PIPE_BUF does not work.

   Similarly for the complete first line, including both newlines, of
   emacs_long_callback.

   Neet to include both newlines, because PG pattern matches on both.
 *)
(** Print [cmd] as emacs callback command. *)
let emacs_callback cmd =
  (* Printf.fprintf (debugc()) "<- %s\n%!" cmd; *)
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


(* 23 chars + goal_id + state + proof_name *)
(** Request Show Goal <ID> at <state> from PG. The proof name is
    needed to associate the output with a proof, because the command
    can be delayed arbitrarily.
*)
let emacs_send_show_goal proof_name state goal_id =
  (* Printf.fprintf (debugc()) "request %s at state %d\n%!" goal_id state; *)
  emacs_callback (Printf.sprintf "show-goal \"%s\" at %d for \"%s\""
                    goal_id state proof_name)

(** Send [confirm-proof_complete] that tells PG that no more show goal
    commands will follow.
*)
let emacs_confirm_proof_complete proof_name =
  emacs_callback (Printf.sprintf "confirm-proof-complete \"%s\"" proof_name)
