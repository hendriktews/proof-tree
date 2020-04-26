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


(** Reading commands from nonblocking stdin *)

open Evar_types

(* for documentation, see input.ml *)

(** Take the necessary actions when the configuration record changed.
*)
val configuration_updated : unit -> unit


(** Parse Coq existential variable information. *)
val coq_evar_parser : string -> (evar_info list * string list)


(** Initialize this module and setup the GTK main loop callback for
    [stdin]. Puts [stdin] into non-blocking mode.
*)
val setup_input : unit -> unit
