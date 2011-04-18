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
 * $Id: main.ml,v 1.6 2011/04/18 07:20:30 tews Exp $
 *)



(** Main --- Argument parsing and start *)


module U = Unix
open Input

let arguments = Arg.align [
  ("-geometry", Arg.Set_string Configuration.geometry_string,
   " X geometry");
  ("-tee", Arg.String (fun s -> Configuration.tee_input_file := Some s),
   "file save input in file");
  ("-debug", Arg.Set Configuration.debug,
   " print more details on errors");
]

let anon_fun s = 
  Printf.eprintf "unrecognized argument %s\n" s;
  exit 1


let main () =
  Arg.parse arguments anon_fun "prooftree";
  setup_input();
  Printf.printf 
    ("Prooftree version %s awaiting input on stdin.\n" ^^
	"Entering LablGTK main loop ...\n\n%!")
    Version.version;
  GMain.Main.main ()


let main_ex () =
  try
    Printexc.record_backtrace true;
    main()
  with
    | e ->
      let backtrace = Printexc.get_backtrace() in
      prerr_string "\nFatal error: escaping exception ";
      prerr_endline (Printexc.to_string e);
      (match e with
	| U.Unix_error(error, _func, _info) ->
	  Printf.eprintf "%s\n" (U.error_message error)      
	| _ -> ()
      );
      prerr_endline "";
      prerr_string backtrace;
      prerr_endline "";
      exit 2

let _ = main_ex()

