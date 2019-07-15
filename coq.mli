(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2019 Hendrik Tews
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

(** Coq specific evar parsing code *)

(** Parse the information display for existential variables of Coq.
    This information can look like one of the folling lines:

    [(dependent evars:)]

    [(dependent evars: ?35 open, ?36 using ?42 ?41 , ?42 open,) ]

    [(dependent evars: ?35 open, ?36 using ?42 ?41 , ?42 using ,)]

    This function returns a tuple, where the first element is the list
    of open, uninstantiated existential variables. The second element
    is a list of pairs, where each pair contains an instantiated
    existential variable and the list of variables that are used in its
    instantiation.

    If parsing dies with an exception, a suitable error dialog is 
    displayed.
*)
val coq_parse_existential_info :
  string -> (string list * (string * string list) list)
