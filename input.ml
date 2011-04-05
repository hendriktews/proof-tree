(* 
 * start-tree coq name-bytes %d <data>
 * 
 * sequent %d sequent-bytes %d <data>
 * 
 * proof-step command-bytes %d <data>
 * 
 * apparently-finished
 * 
 * switch-to %d
 * 
 * proof-completed
 *)

open Util

module U = Unix

exception Protocol_error of string


let match_proof_assistant = function
  | "coq" -> ()
  | pa -> 
    raise (Protocol_error ("unknown proof assistant " ^ pa))


let read_command_line_parser = ref (fun () -> ())

let current_parser = ref (fun () -> ())

let command_buffer_len = 100

let command_buffer = String.create command_buffer_len

let command_buffer_index = ref 0

let init_string len =
  let s = String.create len in
  let n = min len !command_buffer_index in
  let new_command_buffer_index = !command_buffer_index - n in
  String.blit command_buffer 0 s 0 n;
  String.blit command_buffer n command_buffer 0 new_command_buffer_index;
  command_buffer_index := new_command_buffer_index;
  (* 
   * Printf.eprintf "IS ret %d bytes %s\n|%s|\n%!"
   *   n 
   *   (String.sub s 0 n)
   *   (String.sub command_buffer 0 !command_buffer_index);
   *)
  (s, n)  


(******************************************************************************
 * start-tree coq name-bytes %d <data>
 *)

let rec parse_start_tree_cont proof_name len i () =
  if i = len
  then begin
    (* prerr_endline "dispatch start tree"; *)
    Proof_tree.start proof_name;
    current_parser := !read_command_line_parser;
  end
  else
    let n = input stdin proof_name i (len - i) in
    current_parser := (parse_start_tree_cont proof_name len (n + i))


let parse_start_tree com_buf =
  Scanf.bscanf com_buf " %s name-bytes %d"
    (fun coq len ->
      match_proof_assistant coq;
      let (proof_name, n) = init_string len in
      current_parser := (parse_start_tree_cont proof_name len n)
    )      


(******************************************************************************
 * sequent %d sequent-bytes %d <data>
 *)

let rec parse_sequent_cont sequent_id sequent len i () =
  if i = len
  then begin
    (* prerr_endline "dispatch sequent"; *)
    Proof_tree.add_or_update_sequent sequent_id sequent;
    current_parser := !read_command_line_parser;
  end
  else
    let n = input stdin sequent i (len - i) in
    current_parser := (parse_sequent_cont sequent_id sequent len (n + i))

let parse_sequent com_buf = 
  Scanf.bscanf com_buf " %s sequent-bytes %d"
    (fun sequent_id len ->
      let (sequent, n) = init_string len in
      current_parser := (parse_sequent_cont sequent_id sequent len n)
    )


(******************************************************************************
 * proof-step command-bytes %d <data>
 *)

let rec parse_proof_step_cont proof_command len i () =
  if i = len
  then begin
    (* prerr_endline "dispatch proof step"; *)
    Proof_tree.add_proof_command proof_command;
    current_parser := !read_command_line_parser;
  end
  else
    let n = input stdin proof_command i (len - i) in
    current_parser := (parse_proof_step_cont proof_command len (n + i))

let parse_proof_step com_buf =
  Scanf.bscanf com_buf " command-bytes %d" 
    (fun len ->
      let (proof_command, n) = init_string len in
      current_parser := (parse_proof_step_cont proof_command len n)
    )


(******************************************************************************
 * apparently-finished
 *)

let do_apparently_finished () =
  (* prerr_endline "dispatch apparently-finished"; *)
  Proof_tree.finish_branch ()


(******************************************************************************
 * switch-to %d
 *)

let parse_switch_to com_buf = 
  Scanf.bscanf com_buf " %s"
    (fun id ->
      (* prerr_endline "dispatch switch to"; *)
      Proof_tree.switch_to_sequent id;
      current_parser := !read_command_line_parser	  
    )


(******************************************************************************
 * proof-completed
 *)

let do_proof_completed () =
  (* prerr_endline "dispatch proof completed"; *)
  Proof_tree.finish_proof()

(*****************************************************************************
 *
 * general parsing 
 *
 *****************************************************************************)

let parse_command command =
  let com_buf = Scanf.Scanning.from_string command in
  Scanf.bscanf com_buf "%s " 
    (function
      | "start-tree" -> parse_start_tree com_buf 
      | "sequent" -> parse_sequent com_buf 
      | "proof-step" -> parse_proof_step com_buf 
      | "apparently-finished" -> do_apparently_finished()
      | "switch-to" -> parse_switch_to com_buf 
      | "proof-completed" -> do_proof_completed()
      | x -> raise (Protocol_error ("Unknown command " ^ x))
    )
	

let read_command_line () =
  let old_index = !command_buffer_index in
  let bytes_read = 
    input stdin command_buffer old_index (command_buffer_len - old_index)
  in
  let new_index = old_index + bytes_read in
  command_buffer_index := new_index;
  (* 
   * Printf.eprintf "RCL read %d bytes, com buf:\n|%s|\n%!"
   *   bytes_read (String.sub command_buffer 0 !command_buffer_index);
   *)
  match search_char command_buffer 0 new_index '\n' with
    | None ->
      if new_index = 0 
      then
	raise End_of_file
      else if new_index = command_buffer_len
      then
	raise (Protocol_error "No newline in command line")
      else ()
    | Some i ->
      let command = String.sub command_buffer 0 i in
      let rest_index = new_index - i - 1 in
      String.blit command_buffer (i + 1) command_buffer 0 rest_index;
      command_buffer_index := rest_index;
      (* 
       * Printf.eprintf "RCL 2 |%s|\n%!"
       * 	(String.sub command_buffer 0 !command_buffer_index);
       *)
      parse_command command

let parse_input () =
  try
    while true do
      !current_parser ()
    done;
    true
  with
    | Sys_blocked_io -> 
      Proof_tree.finish_drawing ();
      true


let parse_input_callback = function
  | [`IN] -> parse_input ()
  | clist ->
    raise (Protocol_error 
	     ("Strange callback condition " ^
		 (String.concat " "
		    (List.map (function
		      | `IN   -> "IN"
		      | `OUT  -> "OUT"
		      | `PRI  -> "PRI"
		      | `ERR  -> "ERR"
		      | `HUP  -> "HUP"
		      | `NVAL -> "NVAL")
		       clist))))

let error_counter = ref 0

let parse_input_callback_ex clist =
  try
    parse_input_callback clist
  with
    | e ->
      incr error_counter;
      if !error_counter > 20 then exit 2;
      let backtrace = Printexc.get_backtrace() in
      let buf = Buffer.create 4095 in
      (match e with
	| Protocol_error err ->
	  Printf.bprintf buf "Protocol error %s\nClosing connection." err
	| End_of_file ->
	  Buffer.add_string buf "Connection closed."
	| e ->
	  Buffer.add_string buf
	    "Internal error: Escaping exception in parse_input\n";
	  Buffer.add_string buf (Printexc.to_string e);
	  (match e with
	    | U.Unix_error(error, _func, _info) ->
	      Buffer.add_string buf (U.error_message error);
	      Buffer.add_string buf "\n"
	    | _ -> ()
	  );
	  Buffer.add_string buf "\n";
	  Buffer.add_string buf backtrace;
      );
      prerr_endline (Buffer.contents buf);
      error_message_dialog (Buffer.contents buf);
      false
	

(*****************************************************************************
 *
 * Initialization
 *
 *****************************************************************************)

let setup_input () =
  U.set_nonblock U.stdin;
  read_command_line_parser := read_command_line;
  current_parser := read_command_line;
  ignore(GMain.Io.add_watch 
	   ~cond:[ `IN ]
	   ~callback:parse_input_callback_ex
	   (GMain.Io.channel_of_descr U.stdin))
