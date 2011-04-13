(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 Hendrik Tews
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the
 * parent directories for more details.
 * 
 * $Id: input.ml,v 1.4 2011/04/13 07:56:47 tews Exp $
 * 
 * Commentary: reading/parsing commands from nonblocking stdin
 *)


(*****************************************************************************
 *****************************************************************************
 * 
 * The communication protocol with Proof General is one-way (only 
 * Proof General sends commands to prooftree). All data is UTF-8 
 * encoded. Prooftree understands the following 7 commands in the
 * following format:
 * 
 * 
 *   start-tree Coq state %d name-bytes %d\n<data>
 *   
 *   sequent %s state %d sequent-bytes %d\n<data>
 *   
 *   proof-step state %d command-bytes %d\n<data>
 *   
 *   apparently-finished state %d
 *   
 *   switch-to %d state %d
 *   
 *   proof-completed state %d
 *   
 *   undo-up-to state %d
 * 
 * 
 * Here ``%d'' stands for a positive integer. Following the keyword 
 * state it is a state number. Following a keyword xxx-bytes it denotes 
 * the number of bytes of the following <data>, including the final newline 
 * in <data>.
 * 
 * 
 *****************************************************************************
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


(******************************************************************************
 * start-tree Coq state %d name-bytes %d <data>
 *)

let rec parse_start_tree_cont state proof_name len i () =
  if i = len
  then begin
    (* prerr_endline "dispatch start tree"; *)
    Proof_tree.start state proof_name;
    current_parser := !read_command_line_parser;
  end
  else
    let n = local_input proof_name i (len - i) in
    current_parser := (parse_start_tree_cont state proof_name len (n + i))


let parse_start_tree com_buf =
  Scanf.bscanf com_buf " %s state %d name-bytes %d"
    (fun coq state len ->
      match_proof_assistant coq;
      let (proof_name, n) = init_string len in
      current_parser := (parse_start_tree_cont state proof_name len n)
    )      


(******************************************************************************
 * sequent %s state %d sequent-bytes %d <data>
 *)

let rec parse_sequent_cont sequent_id state sequent len i () =
  if i = len
  then begin
    (* prerr_endline "dispatch sequent"; *)
    Proof_tree.add_or_update_sequent state sequent_id sequent;
    current_parser := !read_command_line_parser;
  end
  else
    let n = local_input sequent i (len - i) in
    current_parser := (parse_sequent_cont sequent_id state sequent len (n + i))

let parse_sequent com_buf = 
  Scanf.bscanf com_buf " %s state %d sequent-bytes %d"
    (fun sequent_id state len ->
      let (sequent, n) = init_string len in
      current_parser := (parse_sequent_cont sequent_id state sequent len n)
    )


(******************************************************************************
 * proof-step state %d command-bytes %d <data>
 *)

let rec parse_proof_step_cont state proof_command len i () =
  if i = len
  then begin
    (* prerr_endline "dispatch proof step"; *)
    Proof_tree.add_proof_command state proof_command;
    current_parser := !read_command_line_parser;
  end
  else
    let n = local_input proof_command i (len - i) in
    current_parser := (parse_proof_step_cont state proof_command len (n + i))

let parse_proof_step com_buf =
  Scanf.bscanf com_buf " state %d command-bytes %d" 
    (fun state len ->
      let (proof_command, n) = init_string len in
      current_parser := (parse_proof_step_cont state proof_command len n)
    )


(******************************************************************************
 * apparently-finished state %d
 *)

let do_apparently_finished com_buf =
  Scanf.bscanf com_buf " state %d"
    (fun state ->
      (* prerr_endline "dispatch apparently-finished"; *)
      Proof_tree.finish_branch state;
      current_parser := !read_command_line_parser	  
    )


(******************************************************************************
 * switch-to %d state %d
 *)

let parse_switch_to com_buf = 
  Scanf.bscanf com_buf " %s state %d"
    (fun id state ->
      (* prerr_endline "dispatch switch to"; *)
      Proof_tree.switch_to_sequent state id;
      current_parser := !read_command_line_parser	  
    )


(******************************************************************************
 * proof-completed state %d
 *)

let do_proof_completed com_buf =
  Scanf.bscanf com_buf " state %d"
    (fun state ->
      (* prerr_endline "dispatch proof completed"; *)
      Proof_tree.finish_proof state;
      current_parser := !read_command_line_parser	  
    )


(******************************************************************************
 * undo-up-to state %d
 *)

let do_undo com_buf =
  Scanf.bscanf com_buf " state %d"
    (fun state -> 
      Proof_tree.undo state;
      current_parser := !read_command_line_parser	  
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
	| "start-tree" -> parse_start_tree com_buf 
	| "sequent" -> parse_sequent com_buf 
	| "proof-step" -> parse_proof_step com_buf 
	| "apparently-finished" -> do_apparently_finished com_buf
	| "switch-to" -> parse_switch_to com_buf 
	| "proof-completed" -> do_proof_completed com_buf
	| "undo-up-to" -> do_undo com_buf
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
