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
 * $Id: main.ml,v 1.10 2011/07/28 12:53:07 tews Exp $
 *)


(* 
  (setq proof-tree-arguments '("-tee" "/tmp/tews/ptlog")) 
*)

(* let _ = Configuration.tee_input_file := Some "/tmp/tews/proof-tree-input" *)


(** Main --- Argument parsing and program start *)


(**/**)
module U = Unix
(**/**)
open Configuration
open Input

(** Argument list for [Arg.parse] *)
let arguments = Arg.align [
  ("-geometry", Arg.Set_string Configuration.geometry_string,
   " X geometry");
  ("-tee", 
   Arg.String (fun s -> 
     current_config := 
       {!current_config with 
	 copy_input = true;
	 copy_input_file = s}
   ),
   "file save input in file");
  ("-debug", 
   Arg.Unit (fun () ->
     current_config := {!current_config with debug_mode = true}
   ),
   " print more details on errors");
]

(** Function for anonymous arguments. Terminates the program with 
    exit status 1.
*)
let anon_fun s = 
  Printf.eprintf "unrecognized argument %s\n" s;
  exit 1


(** Main function without exception handling. Performs the following actions:
- parses command line arguments
- registers {!Input.parse_input_callback_ex} as callback for [stdin] 
  in the GTK main loop
- print a hello world message to [stdout]
- start the GTK main loop
*)
let main () =
  Arg.parse arguments anon_fun "prooftree";
  setup_input();
  Printf.printf 
    ("Prooftree version %s awaiting input on stdin.\n" ^^
	"Entering LablGTK main loop ...\n\n%!")
    Version.version;
  GMain.Main.main ()


(** Real main function, which is just an exception handling wrapper 
    around [main].
*)
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

