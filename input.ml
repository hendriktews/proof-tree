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
 * $Id: input.ml,v 1.21 2011/10/28 15:07:30 tews Exp $
 *)


(** Reading commands from nonblocking stdin *)


(*****************************************************************************
 *****************************************************************************)
(** {2 Communition Protocol with Proof General}

    The communication protocol with Proof General is one-way: Only
    Proof General sends commands to prooftree. In response to the 
    commands [prooftree] changes its display. All commands consist of 
    a first line and a possible list of data sections separated by 
    newlines. The first line starts with a distinguishing keyword and 
    contains the length (in bytes including the final newline) of each 
    data section. All data is UTF-8 encoded. 

    In the following list 
    of commands, ``%d'' stands for a positive integer and %s for a string
    which contains no white space. ``\{cheated|not-cheated\}'' denotes
    the alternative of either ``cheated'' or ``not-cheated''. An
    integer following the keyword state is a state number. An integer
    following some xxx-bytes denotes the number of bytes of the next
    <data> section, including the final newline of that <data> section.
    A ``[ \ ]'' at the end of a line denotes line continuation without 
    newline.
    
    Prooftree understands the following commands in the following format:
    {ul

    {-  {v current-goals state %d current-sequent %s \
       {cheated|not-cheated} proof-name-bytes %d command-bytes %d \
       sequent-text-bytes %d existential-bytes %d \
       additional-id-bytes %d\n\
       <data-proof-name>\n\
       <data-command>\n\
       <data-current-sequent>\n\
       <data-additional-ids>\n\
       <data-uninstantiated-existentials>\n v}

       The [current-goals] command tells [prooftree] about a new proof 
       state with a new set of open goals. This corresponds to either of
       the following cases:
       {ul
         {- The initial proof state of a newly started proof}
         {- A proof command has been applied to the old current sequent, 
            yielding a new current sequent and possibly additional new 
            open subgoals}
         {- The old current goal has been solved (by some proof command) 
            and the new current sequent is one of the previously spawned 
            subgoals}
       }
    
       [prooftree] decides with the help of its internal state which of 
       the cases applies.
    
       The set of open goals does not need to represent the total 
       set of all open subgoals, but it must contain all newly 
       spawned subgoals.
    
       The state number in the [current-goals] command is for undo. It 
       is interpreted as the state that has been reached after processing 
       the current command. 
       [current-sequent %s] denotes the ID of the current sequent. The 
       cheated flag tells [prooftree] whether the new proof state was 
       obtained by a cheating command such as [admit] or [sorry]. 
       The data sections are :
       {ol
    	 {- Full name of the proof}
    	 {- The proof command that yielded this proof state}
         {- Text of the current sequent}
         {- ID's of additionally open sequents (as space separated 
            list of strings)}
         {- Space separated list of not yet instantiated existential 
            variables.}
       }
    
       The second data section is ignored for initial proof states.

       The text of newly created additional goals other then the 
       current goal is expected to arrive separately with an 
       [update-sequent] command.
    }
    {- {v update-sequent state %d sequent %s proof-name-bytes %d \
       sequent-text-bytes %d\n\
       <data-proof-name>\n\
       <data-sequent>\n v}
    
       The update sequent command updates the text of some 
       known sequent. Such updates are necessary for newly spawned 
       subgoals. But also when existential variables get instantiated.
    
       The state number is for undo and the sequent ID denotes the 
       sequent to update. The data sections are:
       {ol
    	 {- Full name of the proof}
         {- new sequent text}
       }
    }
    {- {v switch-goal state %d sequent %s proof-name-bytes %d\n
       <data-proof-name>\n v}
    
       Switch goal tells [proftree] that the current goal has changed 
       without changing or solving the old current goal.
    
       The state number is for undo and the only data section is:
       {ol
    	 {- Full name of the proof}
       }
    }
    {- {v proof-complete state %d {cheated|not-cheated} \
       proof-name-bytes %d command-bytes %d\n\
       <data-proof-name>\n\
       <data-command>\n v}
    
       [proof-complete] tells [prooftree] the last proof command that 
       finished the current proof. The cheated flag tells [prooftree]
       whether the new proof state was obtained by a cheating command 
       such as [admit] or [sorry]. The data sections are :
       {ol
    	 {- Full name of the proof}
    	 {- The last proof command}
       }
    }
    {- {v undo-to state %d\n v}
    
       The state number here is not for undo, it is the undo-state.
       Undo tells [prooftree] to change the display to the state before 
       the first command with a state strictly greater than [undo-state]
       has been processed.
    }
    {- {v quit-proof proof-name-bytes %d\n\
        <data-proof-name>\n v}
    
       Quit closes the window for the indicated proof.
       The only data section is:
       {ol
    	 {- Full name of the proof whoose window should be delected}
       }
    }
    }


    {2 General remarks}

    This module reads commands from a pipe. It may therefore happen
    that the input buffer depletes in the middle of a command. In this
    case we have to return control to the GTK main loop, which will
    call this module again, if the operating system decides that it is
    time to make more input available. The input channel is therefore
    turned into non-blocking mode, which means that reading raises an
    exception instead of blocking when currently no more input is
    available. As a consequence, the parsing engine in this module
    must be prepared to get interrupted whenever it tries to read from
    the input channel.

    The state of the parser is stored in three variables. The first
    one, {!Input.current_parser}, holds the function to be called when
    more input becomes available. It must always be set before new
    input is read from the input channel. Typically, there are
    partially filled buffers and index variables in the closure of
    [current_parser]. 

    The remaining two state variables are {!Input.command_buffer} and
    {!Input.command_buffer_index}. They hold the data when the parser
    is reading the first line of a command. In this case
    [current_parser] is set to {!Input.read_command_line}.

    Except for the first line of a command, the communication protocol
    is designed such that the parser always knows in advance how many
    bytes it must read. The first line of any command is terminated by
    a newline (of course) and holds all the size information for the
    remaining parts of the command.
*)
(*****************************************************************************
*****************************************************************************)


open Configuration
open Util
open Gtk_ext

(**/**)
module U = Unix
(**/**)


(** {2 Module Documentation} 
    {3 General parsing utilities and parser state}
*)


(** Exception raised if [prooftree] encounters an unknown or malformed command.
    The first argument is a description of the error. If the error was caused 
    by an exception, the second argument carries this exception and the 
    execption backtrace until the point where [Protocol_error] was raised.
*)
exception Protocol_error of string * (exn * string) option


(* 
 * (\** Check and configure for a specific proof assistant. Currently only
 *     ["Coq"] is supported and no Coq specific configuration is done here.
 * *\)
 * let match_proof_assistant = function
 *   | "Coq" -> ()
 *   | pa -> 
 *     raise (Protocol_error ("unknown proof assistant " ^ pa, None))
 *)


(** Forward pointer to {!Input.read_command_line}. Initialized in 
    {!Input.setup_input}. The forward pointer is needed, because various 
    functions that must be defined before [read_command_line] must set 
    {!Input.current_parser} to [read_command_line].
*)
let read_command_line_parser = ref (fun () -> ())


(** Parsing function to be called when the next input arrives. Typically
    the closure of this function contains the parsing state, such as
    partially filled buffers.
*)
let current_parser = ref (fun () -> ())


(** Length of {!Input.command_buffer}. Must be larger than the first line of 
    any command.
*)
let command_buffer_len = 4096


(** Buffer for the first line of the next command. The part between [0] and
    (excluding) {!Input.command_buffer_index} contains valid input. The buffer
    may actually may hold more than the first line. Therefore, other parsing 
    functions must take input from this buffer before reading on the 
    input channel. Any function that takes content out of [command_buffer]
    must shift the remaining valid input to the left and update 
    [command_buffer_index]. See {!Input.init_string}.
*)
let command_buffer = String.create command_buffer_len


(** First free position in {!Input.command_buffer}. *)
let command_buffer_index = ref 0


(** Create and initialize a parsing buffer (string) of length [len].
    If {!Input.command_buffer} holds valid input, the new parsing
    buffer is filled with that input, as far as possible.
    [command_buffer] and {!Input.command_buffer_index} are properly
    updated if material is taken from there. This function returns the
    newly created buffer together with the index of the next, not yet
    filled byte in the buffer.
*)
let init_string len =
  let s = String.create len in
  let n = min len !command_buffer_index in
  let new_command_buffer_index = !command_buffer_index - n in
  String.blit command_buffer 0 s 0 n;
  String.blit command_buffer n command_buffer 0 new_command_buffer_index;
  command_buffer_index := new_command_buffer_index;
  (* 
   * Printf.fprintf (debugc()) "IS ret %d bytes %s\n|%s|\n%!"
   *   n 
   *   (String.sub s 0 n)
   *   (String.sub command_buffer 0 !command_buffer_index);
   *)
  (s, n)  


(** Output channel for saving a backup copy of all material from the input.
    Set by option [-tee], mainly used for debugging.
*)
let input_backup_oc = ref None

let input_backup_filename = ref None

(** Set {!Input.input_backup_oc} according to the current configuration.
*)
let setup_input_backup_channel () =
  if !current_config.copy_input && 
    !input_backup_filename = Some !current_config.copy_input_file
  then ()
  else if !current_config.copy_input = false &&
	 !input_backup_filename = None
  then ()
  else if !current_config.copy_input
  then begin
    input_backup_oc := Some(open_out !current_config.copy_input_file);
    input_backup_filename := Some !current_config.copy_input_file;
  end else begin
    input_backup_oc := None;
    input_backup_filename := None;
  end
    


(** Input function for reading from the input channel. To make the input
    backup feature work (see option [-tee]) input must always be read with 
    this function. Arguments are the same as for {!Pervasives.input}, 
    [local_input] reads at most [len] bytes from [stdin] into buffer [buf], 
    starting at position [start]. Any material read is immediately written 
    to {!Input.input_backup_oc}. Before calling this function,
    {!Input.current_parser} must be set to the parsing continuation function.
    This will be used in case parsing is interrupted now, because now more 
    input is currently available, and control is given back to the GTK main 
    loop. When more input becomes available the GTK main loop calls this 
    module again and the main parsing loop in {!Input.parse_input} continues
    parsing with the function stored in [current_parser].

    @raise Sys_blocked_io when no more input is available currently
*)
let local_input buf start len =
  let read_len = input stdin buf start len in
  (match !input_backup_oc with
    | None -> ()
    | Some oc ->
      output oc buf start read_len;
      flush oc
  );
  read_len


(** Fill buffer [s] and continue parsing with [continuation_fn]. This
    is utility function for {!Input.get_string}. [get_string_cont s i
    len cont ()] reads [len - i] bytes from the input channel and
    stores them in [s] at position [i]. When finished it calles
    [cont]. This function must only be called when
    {!Input.command_buffer} is empty (because it only reads directly
    from the input channel) or when [i = len] (because then [cont] is 
    called directly. This function sets {!Input.current_parser} to itself 
    to continue reading later if not enough input is available now.

    @raise Sys_blocked_io when not enough input is available currently
*)
let rec get_string_cont s i len continuation_fn () =
  if i = len
  then continuation_fn s
  else begin
    current_parser := (get_string_cont s i len continuation_fn);
    let n = local_input s i (len - i) in
    get_string_cont s (i + n) len continuation_fn ()
  end


(** Main input function for strings of length [len]. [get_string len
    cont] creates a new string of length [len] and fills it from
    {!Input.command_buffer} and the input channel. When finished with
    reading, the continuation [cont] is called. This function properly
    deals with parsing interrupts (by setting {!Input.current_parser}
    internally) and initial content in [command_buffer].

    @raise Sys_blocked_io when not enough input is available currently
*)
let get_string len continuation_fn =
  let (s, n) = init_string len in
  get_string_cont s n len continuation_fn ()



(******************************************************************************
 * current-goals state %d current-sequent %s {cheated|not-cheated} \
 * proof-name-bytes %d command-bytes %d sequent-text-bytes %d \
 * additional-id-bytes %d\n\
 * <data-proof-name>\n\
 * <data-command>\n\
 * <data-current-sequent>\n\
 * <data-additional-ids>\n
 *)

(** {3 Current-goals command parser} *)

(** Finish parsing of the [current-goals] command and call
    {!Proof_tree.process_current_goals} to display the new proof
    state. The arguments are the unprocessed strings read from the
    input channel in this order:
    
    @param state state number from the first line of the command
    @param current_sequent_id ID of the current sequent from the first 
           line of the command
    @param cheated_string either "cheated" or "not-cheated" from the 
           first line of the command
    @param proof_name name of the current proof
    @param proof_command text of the last proof command (or garbage if 
           this is the first state of the proof)
    @param current_sequent_text text of the current sequent
    @param additional_ids_string ID's of all currently open goals
*)
let parse_current_goals_finish state current_sequent_id cheated_string 
    proof_name proof_command current_sequent_text 
    additional_ids_string existentials_string =
  let cheated_flag = match cheated_string with
    | "not-cheated" -> false
    | "cheated" -> true
    | _ -> 
      raise(Protocol_error
	      ("Parse error in current-goals command. " ^
		  "Expected \"cheated\" or \"not-cheated\" as 6th word.",
	       None))
  in
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  let current_sequent_text = chop_final_newlines current_sequent_text in
  let additional_ids_string = chop_final_newlines additional_ids_string in
  let additional_ids = string_split ' ' additional_ids_string in
  let existentials_string = chop_final_newlines existentials_string in
  let existentials = string_split ' ' existentials_string in
  current_parser := !read_command_line_parser;
  Proof_tree.process_current_goals state proof_name proof_command cheated_flag
    current_sequent_id current_sequent_text additional_ids existentials
    

(** Start parsing of the [current-goals] command. Extracts elements and 
    string length' from the [Scanf] parsing buffer argument and reads
    all the necessary strings from the input channel. When reading finished 
    {!Input.parse_current_goals_finish} is called.
*)
let parse_current_goals com_buf =
  Scanf.bscanf com_buf 
    (" state %d current-sequent %s %s proof-name-bytes %d "
     ^^ "command-bytes %d sequent-text-bytes %d "
     ^^ "additional-id-bytes %d existential-bytes %d")
    (fun state current_sequent_id cheated_string proof_name_bytes command_bytes 
      sequent_text_bytes additional_id_bytes existential_bytes ->
	get_string proof_name_bytes
	  (fun proof_name ->
	    get_string command_bytes
	      (fun proof_command ->
		get_string sequent_text_bytes
		  (fun current_sequent_text ->
		    get_string additional_id_bytes
		      (fun additional_ids_string ->
			get_string existential_bytes
			  (fun existentials_string ->
			    parse_current_goals_finish state current_sequent_id
			      cheated_string
			      proof_name proof_command current_sequent_text
			      additional_ids_string existentials_string))))))



(******************************************************************************
 * update-sequent state %d sequent %s proof-name-bytes %d \
 * sequent-text-bytes %d\n\
 * <data-proof-name>\n
 * <data-sequent>\n
 *)

(** {3 Update-sequent command parser} *)


(** Finish parsing of the [update-sequent] command and call
    {!Proof_tree.update_sequent} to update the sequent. The 
    arguments are as follows:

    @param state state number
    @param sequent_id ID of sequent to update
    @param proof_name full proof name (as raw data section string)
    @param sequent_text new sequent text (as raw data section string)
*)
let parse_update_sequent_finish state sequent_id proof_name sequent_text =
  let proof_name = chop_final_newlines proof_name in
  let sequent_text = chop_final_newlines sequent_text in
  current_parser := !read_command_line_parser;
  Proof_tree.update_sequent state proof_name sequent_id sequent_text


(** Parse and process a [update-sequent] command. Extracts the state and
    the data section length' from the first command line in the [Scanf]
    parsing buffer argument, reads the data sections and finally call
    {!Input.parse_update_sequent_finish}.
*)
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
 * switch-goal state %d sequent %s proof-name-bytes %d\n
 * <data-proof-name>\n
 *)

(** {3 Switch-goal command parser} *)


(** Finish parsing of the [switch-goal] command and process it with
    {!Proof_tree.switch_to}. The arguments are as follows:

    @param state state number
    @param new_current_id ID of new current goal
    @param proof_name full proof name (as raw data section string)
*)
let parse_switch_goal_finish state new_current_id proof_name =
  let proof_name = chop_final_newlines proof_name in
  current_parser := !read_command_line_parser;
  Proof_tree.switch_to state proof_name new_current_id


(** Parse and process a [switch-goal] command. Extracts the state, the
    new current sequent and the data section length from the first
    command line in the [Scanf] parsing buffer argument, reads the
    data section and finally calls {!Input.parse_switch_goal_finish}.
*)
let parse_switch_goal com_buf =
  Scanf.bscanf com_buf
    " state %d sequent %s proof-name-bytes %d"
    (fun state new_current_id proof_name_bytes ->
      get_string proof_name_bytes
	(fun proof_name ->
	  parse_switch_goal_finish state new_current_id proof_name))



(******************************************************************************
 * proof-complete state %d {cheated|not-cheated} \
 * proof-name-bytes %d command-bytes %d\n\
 * <data-proof-name>\n\
 * <data-command>\n
*)

(** {3 Proof-complete command parser} *)


(** Finish parsing of the [proof-complete] command and process it
    with {!Proof_tree.process_proof_complete}. The arguments are

    @param state state number
    @param cheated_string either "cheated" or "not-cheated"
    @param proof_name full proof name (as raw data section string)
    @param proof_command last proof command (as raw data section string)
*)
let parse_proof_complete_finish state cheated_string proof_name proof_command =
  let cheated_flag = match cheated_string with
    | "not-cheated" -> false
    | "cheated" -> true
    | _ -> 
      raise(Protocol_error
	      ("Parse error in proof-complete command. " ^
		  "Expected \"cheated\" or \"not-cheated\" as 4th word.",
	       None))
  in
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  current_parser := !read_command_line_parser;
  Proof_tree.process_proof_complete state proof_name proof_command cheated_flag


(** Parse and process a [proof-complete] command. Extracts the
    necessary information from the first command line in the [Scanf]
    parsing buffer argument, reads the data section and finally calls
    {!Input.parse_proof_complete_finish}.
*)
let parse_proof_complete com_buf =
  Scanf.bscanf com_buf " state %d %s proof-name-bytes %d command-bytes %d"
    (fun state cheated_string proof_name_bytes command_bytes ->
      get_string proof_name_bytes 
	(fun proof_name ->
	  get_string command_bytes
	    (fun proof_command ->
	      parse_proof_complete_finish 
		state cheated_string proof_name proof_command)))



(******************************************************************************
 * undo-to state %d\n
 *)

(** {3 Undo-to command parser} *)


(** Parse an [undo-to] command and call {!Proof_tree.undo} to process it.
*)
let do_undo com_buf =
  Scanf.bscanf com_buf " state %d"
    (fun state -> 
      current_parser := !read_command_line_parser;
      Proof_tree.undo state
    )



(*****************************************************************************
 *
 * quit-proof proof-name-bytes %d\n\
 * <data-proof-name>\n
 *)

(** {3 Quit-proof command parser} *)


(** Finish parsing a [quit-proof] command and process it with
    {!Proof_tree.quit_proof}. The argument is

    @param proof_name full proof name (as raw data section string)
*)
let parse_quit_proof_finish proof_name =
  let proof_name = chop_final_newlines proof_name in
  current_parser := !read_command_line_parser;
  Proof_tree.quit_proof proof_name


(** Parse and process a [quit-proof] command. Extracts the
    data-section length from the first line in the [Scanf] parsing
    buffer, reads the data section and finally calls
    {!Input.parse_quit_proof_finish}.
*)
let parse_quit_proof com_buf =
  Scanf.bscanf com_buf " proof-name-bytes %d"
    (fun proof_name_bytes ->
      get_string proof_name_bytes
	(fun proof_name ->
	  parse_quit_proof_finish proof_name))



(*****************************************************************************
 *
 * general parsing 
 *
 *****************************************************************************)

(** {3 General command parser} *)

(** Parse and process a command. Argument [command] holds the complete
    first line of the command. This function only builds a scanning
    buffer from [command] and switches to the different command
    parsers, depending on the first word in [command].

    @raise Protocol_error in case the first word is not recognized or
           some other protocol error occured
*)
let parse_command command =
  (* Printf.fprintf (debugc()) "PC %s\n%!" command; *)
  let com_buf = Scanf.Scanning.from_string command in
  try
    Scanf.bscanf com_buf "%s " 
      (function
	| "current-goals" -> parse_current_goals com_buf
	| "update-sequent" -> parse_update_sequent com_buf
	| "switch-goal" -> parse_switch_goal com_buf
	| "proof-complete" -> parse_proof_complete com_buf
	| "undo-to" -> do_undo com_buf
	| "quit-proof" -> parse_quit_proof com_buf
	| x -> 
	  raise (Protocol_error ("Parse error on input \"" ^ command ^ "\"",
				 None))
      )
  with
    | Scanf.Scan_failure _msg as e ->
      let bt = Printexc.get_backtrace() in
      raise (Protocol_error ("Parse error on input \"" ^ command ^ "\"", 
			     Some(e, bt)))


(** Read first line of a command (and parse and process the entire
    command). This function is the entry point into the command
    parser. It reads input into {!Input.command_buffer} until it can
    extract the first line of a command. It then calls
    {!Input.parse_command} to finish parsing and process the command.
    All command parsing functions set {!Input.current_parser} to
    [read_command_line] when they are finished with their work. This
    way, this function is called again to parse the next command by
    the main parsing loop in {!Input.parse_input}.

    @raise Sys_blocked_io if parsing gets interrupted
    @raise Protocol_error for parsing and protocol errors
*)
let read_command_line () =
  (* Printf.fprintf (debugc()) "RCL start\n%!"; *)
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
   * Printf.fprintf (debugc()) "RCL read %d bytes, com buf:\n|%s|\n%!"
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
       * Printf.fprintf (debugc()) "RCL 2 |%s|\n%!"
       * 	(String.sub command_buffer 0 !command_buffer_index);
       *)
      parse_command command



(** {3 Main parsing loop, GTK callback and initialization} *)

(** Main parsing loop. Calls the function in {!Input.current_parser}
    in an infinite loop until input depletes and parsing is
    interrupted. If parsing is interrupted this function calls
    {!Proof_tree.finish_drawing} to schedule a redisplay of the proof
    tree if necessary.

    All parsing functions raise [Sys_blocked_io] if no more input is
    available. This exception is caught here.

    This function always returns [true] to tell the GTK main loop to
    keep calling this module.

    @raise Protocol_error for parsing and protocol errors
*)
let parse_input () =
  try
    (* Printf.fprintf (debugc()) "PI first\n%!"; *)
    while true do
      (* Printf.fprintf (debugc()) "PI next\n%!"; *)
      !current_parser ()
    done;
    true
  with
    | Sys_blocked_io -> 
      (* Printf.fprintf (debugc()) "PI finished\n%!"; *)
      Proof_tree.finish_drawing ();
      true


(** Input callback without exception handling. The argument comes from
    the GTK main loop and indicates the condition on the watched
    channel. Because I only registered a callback for the [`IN] (i.e.,
    input available) condition, I only deal with the case where the
    argument is [[`IN]]. For all other arguments
    {!Input.Protocol_error} is thrown, because I either don't expect
    them or I don't know what they mean.
*)
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


(** Internal counter of fatal error conditions of the command
    processing. If a fatal error occurs, it is normally displayed in a
    popup message. Special circumstances might cause a fatal error to
    repeately occur. Then this counter causes prooftree to terminate
    at some state instead of generating an infinite number of popup
    messages
*)
let error_counter = ref 0


(** Callback for the GTK main loop when input is available on [stdin].
    This is just an exception wrapper around
    {!Input.parse_input_callback}. In case of an escaping exception a
    popup message is displayed and the same message is printed on
    [stderr]. For {!Input.Protocol_error} and [End_of_input] the
    message only contains a backtrace if {!Configuration.debug} is
    true. For other exceptions the message does always contain the
    backtrace.
*)
let parse_input_callback_ex clist =
  try
    parse_input_callback clist
  with
    | e ->
      incr error_counter;
      if !error_counter > 20 then exit 2;
      let backtrace = Printexc.get_backtrace() in
      let buf = Buffer.create 4095 in
      let print_backtrace = ref !current_config.debug_mode in
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
      error_message_dialog (Buffer.contents buf)
	

(*****************************************************************************
 *
 * Initialization
 *
 *****************************************************************************)

(** Take the necessary actions when the configuration record changed.
*)
let configuration_updated = setup_input_backup_channel

(** Initialize this module and setup the GTK main loop callback for
    [stdin]. Puts [stdin] into non-blocking mode.
*)
let setup_input () =
  U.set_nonblock U.stdin;
  read_command_line_parser := read_command_line;
  current_parser := read_command_line;
  setup_input_backup_channel();
  ignore(GMain.Io.add_watch 
	   ~cond:[ `IN ]
	   ~callback:parse_input_callback_ex
	   (GMain.Io.channel_of_descr U.stdin))
