
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

