(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011, 2012 Hendrik Tews
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
 * $Id: emacs_commands.ml,v 1.3 2013/01/20 21:55:54 tews Exp $
 *)


(** Generate and output emacs callback requests *)


(** Print [cmd] as emacs callback command. *)
let emacs_callback cmd =
  Printf.printf "\nemacs exec: %s\n%!" cmd


(** Issue the stop-displaying emacs callback command. *)
let emacs_callback_stop_display () =
  emacs_callback "stop-displaying"


(** Send an undo command to emacs. *)
let emacs_callback_undo undo_state =
  emacs_callback (Printf.sprintf "undo %d" undo_state)
