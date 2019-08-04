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

(** Types for existential variable parsing *)

(** XXX *)
type evar_info =
  | Noninstantiated of string * string
  | Instantiated of string * string list

(** XXX *)
let print_evar_info oc = function
  | Noninstantiated(int_ex, ex_ex) ->
     Printf.fprintf oc "%s open, printed as %s\n" int_ex ex_ex
  | Instantiated(ex, []) -> Printf.fprintf oc "%s inst. (no deps)\n" ex
  | Instantiated(ex, deps) ->
     Printf.fprintf oc "%s inst., dep on %s\n"
       ex (String.concat " " deps)

(** XXX *)
let print_evar_info_list oc = List.iter (print_evar_info oc)

(** XXX *)
let print_current_evar_names oc evars =
  Printf.fprintf oc "current goal: %s\n" (String.concat " " evars)
