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
 * $Id: input.mli,v 1.1 2011/04/13 10:47:08 tews Exp $
 *)


(** Reading and commands from nonblocking stdin *)


(*****************************************************************************
 *****************************************************************************)
(** The communication protocol with Proof General is one-way (only 
    Proof General sends commands to prooftree). All data is UTF-8 
    encoded. Prooftree understands the following 7 commands in the
    following format:
    
    
      start-tree Coq state %d name-bytes %d\n<data>
      
      sequent %s state %d sequent-bytes %d\n<data>
      
      proof-step state %d command-bytes %d\n<data>
      
      apparently-finished state %d
      
      switch-to %d state %d
      
      proof-completed state %d
      
      undo-up-to state %d
    
    
    Here ``%d'' stands for a positive integer. Following the keyword 
    state it is a state number. Following a keyword xxx-bytes it denotes 
    the number of bytes of the following <data>, including the final newline 
    in <data>.
*)
(*****************************************************************************
 *****************************************************************************)


(** Turn [stdin] into non-blocking mode and register a callback for
    [stdin] in the GTK main loop. The callback will read and parse 
    the commands on [stdin] and display proof trees.
*)
val setup_input : unit -> unit
