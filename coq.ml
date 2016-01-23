(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2016 Hendrik Tews
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
 * $Id: coq.ml,v 1.5 2016/01/23 12:57:13 tews Exp $
 *)


(** Coq specific code *)


open Gtk_ext

(*****************************************************************************
 *****************************************************************************)
(** {3 Coq existential info parser} *)
(*****************************************************************************
 *****************************************************************************)


(* (dependent evars:)
 * (dependent evars: ?35 open, ?36 using ?42 ?41 , ?42 open,) 
 * (dependent evars: ?35 open, ?36 using ?42 ?41 , ?42 using ,)
 *)

(** This function parses one evar uid without the question mark. *)
let coq_parse_evar_without_question scan_buf =
  Scanf.bscanf scan_buf " %[A-Za-z0-9] " (fun evar -> evar)


(** This function parses the dependency list that follows ``using'',
    looking like

    [ ?42 ?41 ,]

    [ ,]

    When the final comma is encountered the now completely parsed evar
    group is appended to the accumulated results and
    {!coq_parse_next_evar_info} is called as continuation.
*)
let rec coq_parse_evar_dependency scan_buf uninst inst evar deps =
  Scanf.bscanf scan_buf " %c " (function
    | ',' ->
      coq_parse_next_evar_info scan_buf 
	uninst ((evar, List.rev deps) :: inst) ()
    | '?' ->
      let dep = coq_parse_evar_without_question scan_buf in
      coq_parse_evar_dependency scan_buf uninst inst evar (dep :: deps)
    | c ->
      raise (Scanf.Scan_failure
	       (Printf.sprintf
		  ("expected an evar (starting with '?') or ',' "
		   ^^ "for the end of the dependency list; but found '%c'")
		  c))
  )

(** This function parses one evar group, looking like

    [?35 open,]

    [?36 using ?42 ?41 ,]

    [?42 using ,]

    When finished {!coq_parse_next_evar_info} is invoked on the
    updated accumulated results.
*)
and coq_parse_one_evar_info scan_buf uninst inst =
  let evar = coq_parse_evar_without_question scan_buf in
  Scanf.bscanf scan_buf " %[^, ]" (function
    | "open" ->
      Scanf.bscanf scan_buf ", " 
	(coq_parse_next_evar_info scan_buf (evar :: uninst) inst) ()
    | "using" ->
      coq_parse_evar_dependency scan_buf uninst inst evar []
    | x ->
      raise (Scanf.Scan_failure 
	       (Printf.sprintf
		  "expected \"open\" or \"using\" but found \"%s\"" x))
  )
  

(** This function decides whether to continue parsing with reading the
    next evar group or whether the end of evar information has been
    reached. In the latter case the accumulated results are returned. 
*)
and coq_parse_next_evar_info scan_buf uninst inst () =
  Scanf.bscanf scan_buf "%c" (function 
    | '?' -> coq_parse_one_evar_info scan_buf uninst inst
    | ')' -> (List.rev uninst, List.rev inst)
    | c ->
      raise (Scanf.Scan_failure
	       (Printf.sprintf
		  ("expected an evar (starting with '?') or " 
		   ^^ "the end of the evar info (a ')'); but found '%c'")
		  c))
  )


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
let coq_parse_existential_info ex_string =
  let scan_buf = Scanf.Scanning.from_string ex_string in
  try
    Scanf.bscanf scan_buf "(dependent evars: " 
      (coq_parse_next_evar_info scan_buf [] []) ()
  with
    | Scanf.Scan_failure msg ->
      error_message_dialog 
	(Printf.sprintf 
	   ("Coq existential variable info parsing error!\n" 
	    ^^ "The parser died on the input\n  %s\n"
	    ^^ "with an exception Scan_failure with message\n%s")
	   ex_string msg)
    | End_of_file ->
      error_message_dialog
	(Printf.sprintf
	   ("Coq existential variable info parsing error!\n" 
	    ^^ "The parser died on the input\n  %s\n"
	    ^^ "with an End_of_file exception.")
	   ex_string)
    | _ -> assert false


