(* Usage: delay file command
 *)


module U = Unix

let rec search_char buf start stop c =
  if start < stop then
    if Bytes.get buf start = c 
    then Some start
    else search_char buf (start + 1) stop c
  else
    None


let main () =
  let buf_len = 4096 in

  let buf = Bytes.create buf_len in

  let buf_index = ref 0 in
  
  let no_eof_yet = ref true in

  let is = open_in Sys.argv.(1) in

  let (p_r, p_w) = U.pipe () in

  U.set_close_on_exec p_w;

  let pid = 
    U.create_process Sys.argv.(2) [| Sys.argv.(2) |] p_r U.stdout U.stderr
  in

  U.close p_r;
  
  let oc = U.out_channel_of_descr p_w in

  at_exit (fun () -> U.kill pid Sys.sigint);

  while true do
    if !no_eof_yet then begin
      try
	let n = input is buf !buf_index (buf_len - !buf_index) in
	buf_index := !buf_index + n;
      with
	| End_of_file -> no_eof_yet := false
    end;
    let lines_len = 24 in
    let lines = Array.make lines_len None in
    let pos = ref 0 in
    let i = ref 1 in
    while !i < lines_len do
      match search_char buf !pos !buf_index '\n' with
	| Some p ->
	  Printf.printf "%2d: " !i;
	  output stdout buf !pos (p - !pos + 1);
	  lines.(!i) <- Some(p + 1);
	  pos := p + 1;
	  incr i
	| None -> i := lines_len
    done;
    let invalid_answer = ref true in
    let line = ref 0 in
    while !invalid_answer do
      Printf.printf "? %!";
      let answer = read_line() in
      let n = 
	try
	  int_of_string answer 
	with
	  | Failure _ -> 
	    if String.length answer = 0
	    then 1
	    else if String.length answer >= 1 && 
		(answer.[0] = 'q' || answer.[0] = 'Q')
	    then
	      exit 0
	    else -1
      in
      if n > 0 && n < lines_len && lines.(n) <> None then begin
	invalid_answer := false;
	line := n
      end;
    done;
    let out_len = match lines.(!line) with
      | Some x -> x
      | None -> assert false
    in
    output oc buf 0 out_len;
    flush oc;
    Bytes.blit buf out_len buf 0 (!buf_index - out_len);
    buf_index := (!buf_index - out_len);
  done


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
(*** compile-command: "ocamlopt.opt -o delay unix.cmxa delay.ml" ***)
(*** End: ***)
