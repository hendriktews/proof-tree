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
 * $Id: input.mli,v 1.7 2011/07/12 12:39:06 tews Exp $
 *)


(** Reading commands from nonblocking stdin *)


(*****************************************************************************
 *****************************************************************************)
(** The communication protocol with Proof General is one-way (only 
    Proof General sends commands to prooftree). All data is UTF-8 
    encoded. Prooftree understands the following commands in the
    following format:
    
      current-goals state %d current-sequent %s {cheated|not-cheated} \
      proof-name-bytes %d command-bytes %d sequent-text-bytes %d \
      additional-id-bytes %d\n\
      <data-proof-name>\n\
      <data-command>\n\
      <data-current-sequent>\n\
      <data-additional-ids>\n

      update-sequent state %d sequent %s proof-name-bytes %d \
      sequent-text-bytes %d\n\
      <data-proof-name>\n\
      <data-sequent>\n

      switch-goal state %d sequent %s proof-name-bytes %d\n
      <data-proof-name>\n

      proof-complete state %d {cheated|not-cheated} \
      proof-name-bytes %d command-bytes %d\n\
      <data-proof-name>\n\
      <data-command>\n
      
      undo-to state %d\n
    
      quit-proof proof-name-bytes %d\n\
      <data-proof-name>\n
    
    Here ``%d'' stands for a positive integer and %s for a string
    which contains no white space. ``{cheated|not-cheated}'' denotes
    the alternative of either ``cheated'' or ``not-cheated''. An
    integer following the keyword state is a state number. An integer
    following some xxx-bytes denotes the number of bytes of the next
    <data>, including the final newline after <data>.
*)
(*****************************************************************************
 *****************************************************************************)


(** Initialize this module and setup the GTK main loop callback for
    [stdin]. Puts [stdin] into non-blocking mode.
*)
val setup_input : unit -> unit
