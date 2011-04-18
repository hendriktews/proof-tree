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
 * $Id: input.ml,v 1.7 2011/04/18 07:20:30 tews Exp $
 *)


(** Reading and commands from nonblocking stdin *)


(*****************************************************************************
 *****************************************************************************)
(** The communication protocol with Proof General is one-way (only 
    Proof General sends commands to prooftree). All data is UTF-8 
    encoded. Prooftree understands the following commands in the
    following format:
    
      current-goals state %d current-sequent %s proof-name-bytes %d \
      command-bytes %d sequent-text-bytes %d additional-id-bytes %d\n\
      <data-proof-name>\n\
      <data-command>\n\
      <data-current-sequent>\n\
      <data-additional-ids>\n

      update-sequent state %d sequent %s proof-name-bytes %d \
      sequent-text-bytes %d\n\
      <data-proof-name>\n\
      <data-sequent>\n

      proof-complete state %d proof-name-bytes %d command-bytes %d\n\
      <data-proof-name>\n\
      <data-command>\n
      
      undo-to state %d\n
    
    Here ``%d'' stands for a positive integer and %s for a string
    which contains no white space. Following the keyword state the
    integer is a state number. Following a keyword xxx-bytes it
    denotes the number of bytes of the following <data>, including the
    final newline after <data>.
*)
(*****************************************************************************
*****************************************************************************)


open Util
open Gtk_ext

module U = Unix

exception Protocol_error of string * (exn * string) option


let match_proof_assistant = function
  | "Coq" -> ()
  | pa -> 
    raise (Protocol_error ("unknown proof assistant " ^ pa, None))


let read_command_line_parser = ref (fun () -> ())

let current_parser = ref (fun () -> ())

let command_buffer_len = 4096

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

let input_backup_oc = ref None

let local_input buf start len =
  let read_len = input stdin buf start len in
  (match !input_backup_oc with
    | None -> ()
    | Some oc ->
      output oc buf start read_len;
      flush oc
  );
  read_len


let rec get_string_cont s i len continuation_fn () =
  if i = len
  then continuation_fn s
  else begin
    current_parser := (get_string_cont s i len continuation_fn);
    let n = local_input s i (len - i) in
    get_string_cont s (i + n) len continuation_fn ()
  end

let get_string len continuation_fn =
  let (s, n) = init_string len in
  get_string_cont s n len continuation_fn ()


(******************************************************************************
 * current-goals state %d current-sequent %s proof-name-bytes %d \
 * command-bytes %d sequent-text-bytes %d additional-id-bytes %d\n\
 * <data-proof-name>\n
 * <data-command>\n
 * <data-current-sequent>\n
 * <data-additional-ids>\n
 *)

let parse_current_goals_finish state current_sequent_id proof_name 
    proof_command current_sequent_text additional_ids_string =
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  let current_sequent_text = chop_final_newlines current_sequent_text in
  let additional_ids_string = chop_final_newlines additional_ids_string in
  let additional_ids = string_split ' ' additional_ids_string in
  current_parser := !read_command_line_parser;
  Proof_tree.process_current_goals state proof_name proof_command
    current_sequent_id current_sequent_text additional_ids
    
let parse_current_goals com_buf =
  Scanf.bscanf com_buf 
    (" state %d current-sequent %s proof-name-bytes %d "
     ^^ "command-bytes %d sequent-text-bytes %d additional-id-bytes %d")
    (fun state current_sequent_id proof_name_bytes command_bytes 
      sequent_text_bytes additional_id_bytes ->
	get_string proof_name_bytes
	  (fun proof_name ->
	    get_string command_bytes
	      (fun proof_command ->
		get_string sequent_text_bytes
		  (fun current_sequent_text ->
		    get_string additional_id_bytes
		      (fun additional_ids_string ->
			parse_current_goals_finish state current_sequent_id
			  proof_name proof_command current_sequent_text
			  additional_ids_string)))))


(******************************************************************************
 * update-sequent state %d sequent %s proof-name-bytes %d \
 * sequent-text-bytes %d\n\
 * <data-proof-name>\n
 * <data-sequent>\n
 *)

let parse_update_sequent_finish state sequent_id proof_name sequent_text =
  let proof_name = chop_final_newlines proof_name in
  let sequent_text = chop_final_newlines sequent_text in
  current_parser := !read_command_line_parser;
  Proof_tree.update_sequent state proof_name sequent_id sequent_text

let parse_update_sequent com_buf =
  Scanf.bscanf com_buf
    " state %d sequent %s proof-name-bytes %d sequent-text-bytes %d"
    (fun state sequent_id proof_name_bytes sequent_text_bytes ->
      get_string proof_name_bytes
	(fun proof_name ->
	  get_string sequent_text_bytes
	    (fun sequent_text ->
	      parse_update_sequent_finish state sequent_id 
		proof_name sequent_text)))


(******************************************************************************
 * proof-complete state %d proof-name-bytes %d command-bytes %d\n\
 * <data-proof-name>\n
 * <data-command>\n
 *)

let parse_proof_complete_finish state proof_name proof_command =
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  current_parser := !read_command_line_parser;
  Proof_tree.process_proof_complete state proof_name proof_command

let parse_proof_complete com_buf =
  Scanf.bscanf com_buf " state %d proof-name-bytes %d command-bytes %d"
    (fun state proof_name_bytes command_bytes ->
      get_string proof_name_bytes 
	(fun proof_name ->
	  get_string command_bytes
	    (fun proof_command ->
	      parse_proof_complete_finish state proof_name proof_command)))

(******************************************************************************
 * undo-to state %d\n
 *)

let do_undo com_buf =
  Scanf.bscanf com_buf " state %d"
    (fun state -> 
      current_parser := !read_command_line_parser;
      Proof_tree.undo state
    )


(*****************************************************************************
 *
 * general parsing 
 *
 *****************************************************************************)

let parse_command command =
  (* Printf.eprintf "PC %s\n%!" command; *)
  let com_buf = Scanf.Scanning.from_string command in
  try
    Scanf.bscanf com_buf "%s " 
      (function
	| "current-goals" -> parse_current_goals com_buf
	| "update-sequent" -> parse_update_sequent com_buf
	| "proof-complete" -> parse_proof_complete com_buf
	| "undo-to" -> do_undo com_buf
	| x -> 
	  raise (Protocol_error ("Parse error on input \"" ^ command ^ "\"",
				 None))
      )
  with
    | Scanf.Scan_failure _msg as e ->
      let bt = Printexc.get_backtrace() in
      raise (Protocol_error ("Parse error on input \"" ^ command ^ "\"", 
			     Some(e, bt)))
	

let read_command_line () =
  (* Printf.eprintf "RCL start\n%!"; *)
  let old_index = !command_buffer_index in
  let (blocked, bytes_read) = 
    try
      (false,
       local_input command_buffer old_index (command_buffer_len - old_index))
    with
      | Sys_blocked_io -> (true, 0)
  in
  let new_index = old_index + bytes_read in
  command_buffer_index := new_index;
  (* 
   * Printf.eprintf "RCL read %d bytes, com buf:\n|%s|\n%!"
   *   bytes_read (String.sub command_buffer 0 !command_buffer_index);
   *)
  match search_char command_buffer 0 new_index '\n' with
    | None ->
      if blocked
      then
	raise Sys_blocked_io
      else if bytes_read = 0 
      then
	raise End_of_file
      else if new_index = command_buffer_len
      then
	raise (Protocol_error ("No newline in command line", None))
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
    (* Printf.eprintf "PI first\n%!"; *)
    while true do
      (* Printf.eprintf "PI next\n%!"; *)
      !current_parser ()
    done;
    true
  with
    | Sys_blocked_io -> 
      (* Printf.eprintf "PI finished\n%!"; *)
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
		       clist)),
	     None))

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
      let print_backtrace = ref !Configuration.debug in
      let prev_exception = ref None in
      (match e with
	| Protocol_error(err, prev_e) ->
	  Printf.bprintf buf "Protocol error %s\nClosing connection." err;
	  prev_exception := prev_e
	| End_of_file ->
	  Buffer.add_string buf "Connection closed."
	| e ->
	  Buffer.add_string buf
	    "Internal error: Escaping exception in parse_input";
	  print_backtrace := true;
      );
      if !print_backtrace then begin
	Buffer.add_char buf '\n';
	Buffer.add_string buf (Printexc.to_string e);
	(match e with
	  | U.Unix_error(error, _func, _info) ->
	    Buffer.add_string buf (U.error_message error);
	    Buffer.add_string buf "\n"
	  | _ -> ()
	);
	Buffer.add_char buf '\n';
	Buffer.add_string buf backtrace;
	(match !prev_exception with
	  | None -> ()
	  | Some(e, bt) ->
	    Buffer.add_string buf "Caused by ";
	    Buffer.add_string buf (Printexc.to_string e);
	    Buffer.add_char buf '\n';
	    Buffer.add_string buf bt
	);
      end;
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
  (match !Configuration.tee_input_file with
    | None -> ()
    | Some f ->
      input_backup_oc := Some(open_out f)
  );
  ignore(GMain.Io.add_watch 
	   ~cond:[ `IN ]
	   ~callback:parse_input_callback_ex
	   (GMain.Io.channel_of_descr U.stdin))
