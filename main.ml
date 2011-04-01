
module U = Unix
open Proof_window

let geometry_string = ref ""

let arguments = Arg.align [
  ("-geometry", Arg.Set_string geometry_string,
   " X geometry");
]

let anon_fun s = 
  Printf.eprintf "unrecognized argument %s\n" s;
  exit 1


let main () =
  Arg.parse arguments anon_fun "gtk testbed";
  let _pw = make_proof_window !geometry_string in
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


(*** Local Variables: ***)
(*** compile-command: "ocamlopt.opt -I +lablgtk2 -o prooftree unix.cmxa \***)
(*** lablgtk.cmxa gtkInit.cmx util.ml gtk_ext.ml draw_tree.ml \***)
(*** proof_window.ml main.ml" ***)
(*** End: ***)
