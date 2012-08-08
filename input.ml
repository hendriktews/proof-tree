(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011, 2012 Hendrik Tews
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
 * $Id: input.ml,v 1.32 2012/08/08 20:51:24 tews Exp $
 *)


(** Reading commands from nonblocking stdin *)

(*****************************************************************************
 *****************************************************************************)
(** {2 Communition Protocol with Proof General}

    The communication protocol with Proof General is almost one-way
    only: Proof General sends display messages to Prooftree and
    Prooftree never requests anything from Proof General. Only when
    the proof-tree window of the current proof is closed, Prooftree
    notifies Proof General. The communication protocol is designed
    such that Prooftree always knows in advance how many bytes it has
    to read until the end of a display message.

    All display messages consist of 
    {ul
    {- a first line of exactly 16 bytes (including the new line) of
    the form "second line 157\n", where the number is the length of
    the second line (including its final newline).}
    {- a second line containing the display command and the length of
    the additional data sections, if the command has data sections.}
    {- the data sections (if any), where the last character of each
    data sections is a newline.}
    }

    All data is UTF-8 encoded. 

    Some data sections have a prover specific format. Currently, 
    [prooftree] only supports Coq.

    In the following list 
    of commands, ``%d'' stands for a positive integer and %s for a string
    which contains no white space. ``\{cheated|not-cheated\}'' denotes
    the alternative of either ``cheated'' or ``not-cheated''. An
    integer following the keyword state is a state number. An integer
    following some xxx-bytes denotes the number of bytes of the next
    <data> section, including the final newline of that <data> section.
    A ``[ \ ]'' at the end of a line denotes line continuation without 
    newline.
    
    Prooftree understands the following display commands in the
    following format. The first 16-byte line that preceeds every
    display-command line is ommitted in the following list.
    {ul

    {-  {v configure for "PA" and protocol version NN v}

    Configure Prooftree for proof assistant PA and communication
    protocol version NN. If proof assistant PA or version NN is not
    supported, Prooftree displays an error message and exits. The name
    PA might contain arbitrary characters but no quotation mark ( '"' ).

    There must always be exectly one configure message, which must be
    the first message. 
    }
    {-  {v current-goals state %d current-sequent %s \
    {cheated|not-cheated} proof-name-bytes %d command-bytes %d \
    sequent-text-bytes %d additional-id-bytes %d existential-bytes %d\n\
    <data-proof-name>\n\
    <data-command>\n\
    <data-current-sequent>\n\
    <data-additional-ids>\n\
    <data-existentials>\n v}

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
    {- Prover specific information about existential variables.}
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
    {- {v proof-finished state %d {cheated|not-cheated} \
    proof-name-bytes %d command-bytes %d existential-bytes %d\n\
    <data-proof-name>\n\
    <data-command>\n\
    <data-existentials>\n v}
    
    [proof-finished] tells [prooftree] the last proof command that 
    closed the last subgoal. The cheated flag tells [prooftree]
    whether the new proof state was obtained by a cheating command 
    such as [admit] or [sorry]. The data sections are :
    {ol
    {- Full name of the proof}
    {- The last proof command}
    {- Prover specific information about existential variables.}
    }
    }
    {- {v proof-complete state %d proof-name-bytes %d\n\
    <data-proof-name>\n v}

    [proof-complete] tells Prooftree that the current proof has been
    completed and will further not be updated. The only data section is:
    {ol
    {- Full name of the proof}
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
    Cloned windows are not closed.
    The only data section is:
    {ol
    {- Full name of the proof whoose window should be delected}
    }
    }
    }
*)

(** Version number of the communication protocol described and
    implemented by this module.
*)
let protocol_version = 2



(** {2 General remarks}

    This module reads display commands from a pipe. It may therefore happen
    that the input buffer depletes in the middle of a command. In this
    case we have to return control to the GTK main loop, which will
    call this module again, if the operating system decides that it is
    time to make more input available. The input channel is therefore
    turned into non-blocking mode, which means that reading raises an
    exception instead of blocking when currently no more input is
    available. As a consequence, the parsing engine in this module
    must be prepared to get interrupted whenever it tries to read from
    the input channel.

    The state of the parser is stored in the variable
    {!Input.current_parser}, which holds the function to be called when
    more input becomes available. It must always be set before new
    input is read from the input channel. Typically, there are
    partially filled buffers and index variables in the closure of
    [current_parser]. 
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


(** Parsing function for the info string of existential variables.
    This function is proof assistant specific and must therefore be
    set when the configure message is received in
    {!configure_prooftree}. The default value here is a valid parser
    that can be used for proof assistants that have no existential
    variables.
*)
let parse_existential_info =
  ref(fun _ -> ([], []) : string -> (string list * (string * string list) list))

(** Forward pointer to {!message_start}. Initialized in 
    {!setup_input}. The forward pointer is needed, because various 
    functions that must be defined before [message_start] must set 
    {!current_parser} to [message_start].
*)
let message_start_parser = ref (fun () -> ())


(** Parsing function to be called when the next input arrives. Typically
    the closure of this function contains the parsing state, such as
    partially filled buffers.
*)
let current_parser = ref (fun () -> ())


(** Output channel for saving a backup copy of all material from the input.
    Set by option [-tee], mainly used for debugging.
*)
let input_backup_oc = ref None

(** Filename {!input_backup_oc} is referring to. Needed in order to
    decide whether {!input_backup_oc} must be changed when the current
    configuration changed.
*)
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
    


(** Input function for reading from the input channel. To make the
    input backup feature work (see option [-tee]) input must always be
    read with this function. Arguments are the same as for {xref
    stdlib val Pervasives.input}, [local_input buf start len] reads at
    most [len] bytes from [stdin] into buffer [buf], starting at
    position [start]. Any material read is immediately written to
    {!Input.input_backup_oc}. Before calling this function,
    {!Input.current_parser} must be set to the parsing continuation
    function. This will be used in case parsing is interrupted now,
    because no more input is currently available, and control is given
    back to the GTK main loop. When more input becomes available the
    GTK main loop calls this module again and the main parsing loop in
    {!Input.parse_input} continues parsing with the function stored in
    [current_parser].

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
  if read_len = 0 then
    raise (Protocol_error("Connection closed", None));
  read_len


(** [get_string_cont s i len cont ()] 
    fills buffer [s] and continue parsing with [cont]. This
    is a utility function for {!Input.get_string}. [get_string_cont s i
    len cont ()] reads [len - i] bytes from the input channel and
    stores them in [s] at position [i]. When finished it calles
    [cont]. This function sets {!Input.current_parser} to itself 
    to continue reading later if not enough input is available now.

    @raise Sys_blocked_io when not enough input is available currently
*)
let rec get_string_cont s i len continuation_fn () =
  (* Printf.fprintf (debugc()) "GS cont %d - %d enter\n%!" i len; *)
  current_parser := (get_string_cont s i len continuation_fn);
  let n = local_input s i (len - i) in
  (* 
   * Printf.fprintf (debugc()) "GS read %d bytes: %s\n%!" 
   *   n (String.sub s i n);
   *)
  let i = i + n in
  if i = len
  then begin
    (* Printf.fprintf (debugc()) "GS %d yields %s\n%!" len s; *)
    continuation_fn s
  end
  else get_string_cont s i len continuation_fn ()


(** Main input function for strings. [get_string len cont] creates a
    new string of length [len] and fills it from [stdin], saving a
    copy to {!input_backup_oc}, and calls [cont new_string] as
    continuation when finished. This function properly deals with
    parsing interrupts (by setting {!Input.current_parser}
    internally).

    @raise Sys_blocked_io when not enough input is available currently
*)
let get_string len continuation_fn =
  (* Printf.fprintf (debugc()) "GS %d enter\n%!" len; *)
  let s = String.create len in
  get_string_cont s 0 len continuation_fn ()


(******************************************************************************
 ******************************************************************************
 * configure for "PA" and protocol version NN
 *)

(** {3 Configure command parser} *)

(** [true] if the configure message has been received. *)
let configure_message_received = ref false

(** Raise an error if no configure message has been received yet. *)
let check_if_configured () =
  if not !configure_message_received then
    raise (Protocol_error ("Configure message missing", None))

(** Process the configure message. Raise an error if the proof
    assistant or the communication protocol version is not supported.
    This function is the place were a new proof assistant must be
    added.
*)
let configure_prooftree proof_assistant pg_protocol_version =
  if !configure_message_received then
    raise (Protocol_error ("Received a second configure message", None));
  (match proof_assistant with
    | "Coq" -> 
      parse_existential_info := Coq.coq_parse_existential_info
    | "HOL Light" -> ()
    | _ -> 
      raise (Protocol_error ("Unknown proof assistant " ^ proof_assistant,
			     None))
  );
  if protocol_version <> pg_protocol_version then
    raise (Protocol_error 
	     ((Printf.sprintf
		 ("Communication protocol mismatch.\n"
		  ^^ "Proof General uses version %02d\n"
		  ^^ "but this version of Prooftree supports version %02d")
		 pg_protocol_version protocol_version),
	      None));
  configure_message_received := true


(** Parse the configure message and process it. *)
let parse_configure com_buf =
  Scanf.bscanf com_buf 
    " for \"%s@\" and protocol version %d" configure_prooftree

(******************************************************************************
 ******************************************************************************
 * current-goals state %d current-sequent %s {cheated|not-cheated} \
 * proof-name-bytes %d command-bytes %d sequent-text-bytes %d \
 * additional-id-bytes %d existential-bytes %d\n\
 * <data-proof-name>\n\
 * <data-command>\n\
 * <data-current-sequent>\n\
 * <data-additional-ids>\n\
 * <data-existentials>\n
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
    @param existentials_string prover specific information about 
           existentials

*)
let parse_current_goals_finish state current_sequent_id cheated_string 
    proof_name proof_command current_sequent_text 
    additional_ids_string existentials_string =
  (* Printf.fprintf (debugc()) "PCGF\n%!"; *)
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
  let (ex_uninst, ex_inst) = !parse_existential_info existentials_string in
  Proof_tree.process_current_goals state proof_name proof_command cheated_flag
    current_sequent_id current_sequent_text additional_ids ex_uninst ex_inst;
  current_parser := !message_start_parser

    

(** Start parsing of the [current-goals] command. Extracts elements and 
    string length' from the [Scanf] parsing buffer argument and reads
    all the necessary strings from the input channel. When reading finished 
    {!Input.parse_current_goals_finish} is called.
*)
let parse_current_goals com_buf =
  check_if_configured ();
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
  Proof_tree.update_sequent state proof_name sequent_id sequent_text;
  current_parser := !message_start_parser



(** Parse and process a [update-sequent] command. Extracts the state and
    the data section length' from the first command line in the [Scanf]
    parsing buffer argument, reads the data sections and finally call
    {!Input.parse_update_sequent_finish}.
*)
let parse_update_sequent com_buf =
  check_if_configured ();
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
  Proof_tree.switch_to state proof_name new_current_id;
  current_parser := !message_start_parser


(** Parse and process a [switch-goal] command. Extracts the state, the
    new current sequent and the data section length from the first
    command line in the [Scanf] parsing buffer argument, reads the
    data section and finally calls {!Input.parse_switch_goal_finish}.
*)
let parse_switch_goal com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf
    " state %d sequent %s proof-name-bytes %d"
    (fun state new_current_id proof_name_bytes ->
      get_string proof_name_bytes
	(fun proof_name ->
	  parse_switch_goal_finish state new_current_id proof_name))



(******************************************************************************
 * proof-finished state %d {cheated|not-cheated} \
 * proof-name-bytes %d command-bytes %d existential-bytes %d\n\
 * <data-proof-name>\n\
 * <data-command>\n\
 * <data-existentials>\n
*)

(** {3 Proof-finished command parser} *)


(** Finish parsing of the [proof-finished] command and process it
    with {!Proof_tree.process_proof_finished}. The arguments are

    @param state state number
    @param cheated_string either "cheated" or "not-cheated"
    @param proof_name full proof name (as raw data section string)
    @param proof_command last proof command (as raw data section string)
    @param existentials_string prover specific data about existentials
*)
let parse_proof_finished_finish 
    state cheated_string proof_name proof_command existentials_string =
  let cheated_flag = match cheated_string with
    | "not-cheated" -> false
    | "cheated" -> true
    | _ -> 
      raise(Protocol_error
	      ("Parse error in proof-finished command. " ^
		  "Expected \"cheated\" or \"not-cheated\" as 4th word.",
	       None))
  in
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  let existentials_string = chop_final_newlines existentials_string in
  let (ex_uninst, ex_inst) = !parse_existential_info existentials_string in
  Proof_tree.process_proof_finished 
    state proof_name proof_command cheated_flag ex_uninst ex_inst;
  current_parser := !message_start_parser


(** Parse and process a [proof-finished] command. Extracts the
    necessary information from the first command line in the [Scanf]
    parsing buffer argument, reads the data section and finally calls
    {!Input.parse_proof_finished_finish}.
*)
let parse_proof_finished com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf
    " state %d %s proof-name-bytes %d command-bytes %d existential-bytes %d"
    (fun state cheated_string proof_name_bytes 
                                    command_bytes existential_bytes ->
      get_string proof_name_bytes 
	(fun proof_name ->
	  get_string command_bytes
	    (fun proof_command ->
	      get_string existential_bytes
		(fun existentials_string ->
		  parse_proof_finished_finish 
		    state cheated_string proof_name 
		    proof_command existentials_string))))



(******************************************************************************
 * proof-complete state %d proof-name-bytes %d\n\
 * <data-proof-name>\n
 *)

(** {3 Proof-complete command parser} *)


(** Parse and process a [proof-complete] command. Extracts information
    from the command and process it with
    {!Proof_tree.process_proof_complete}.
 *)
let parse_proof_complete com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf " state %d proof-name-bytes %d"
    (fun state proof_name_bytes ->
      get_string proof_name_bytes 
	(fun proof_name ->
	  let proof_name = chop_final_newlines proof_name in
	  Proof_tree.process_proof_complete state proof_name;
	  current_parser := !message_start_parser))


(******************************************************************************
									       * undo-to state %d\n
*)

(** {3 Undo-to command parser} *)


(** Parse an [undo-to] command and call {!Proof_tree.undo} to process it.
*)
let do_undo com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf " state %d" Proof_tree.undo


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
  Proof_tree.quit_proof proof_name;
  current_parser := !message_start_parser


(** Parse and process a [quit-proof] command. Extracts the
    data-section length from the first line in the [Scanf] parsing
    buffer, reads the data section and finally calls
    {!Input.parse_quit_proof_finish}.
*)
let parse_quit_proof com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf " proof-name-bytes %d"
    (fun proof_name_bytes ->
      get_string proof_name_bytes parse_quit_proof_finish)



(*****************************************************************************
 *
 * general parsing 
 *
 *****************************************************************************)

(** {3 General command parser} *)

(** Parse and process a command. Argument [command] holds the complete
    second line of the display command. This function only builds a scanning
    buffer from [command] and switches to the different command
    parsers, depending on the first word in [command].

    @raise Sys_blocked_io if parsing gets interrupted
*)
let parse_command command =
  (* Printf.fprintf (debugc()) "PC %s\n%!" command; *)
  let com_buf = Scanf.Scanning.from_string command in
  Scanf.bscanf com_buf "%s " 
    (function
      | "configure" -> parse_configure com_buf
      | "current-goals" -> parse_current_goals com_buf
      | "update-sequent" -> parse_update_sequent com_buf
      | "switch-goal" -> parse_switch_goal com_buf
      | "proof-finished" -> parse_proof_finished com_buf
      | "proof-complete" -> parse_proof_complete com_buf
      | "undo-to" -> do_undo com_buf
      | "quit-proof" -> parse_quit_proof com_buf
      | _ -> 
	raise (Protocol_error ("Parse error on input \"" ^ command ^ "\"",
			       None))
    );
  current_parser := !message_start_parser;
  ()


(** [read_second_line first_line] extracts the length of the second
    line from [first_line], reads the second line and switches to
    {!parse_command} to process the complete display command.

    @raise Sys_blocked_io if parsing gets interrupted
*)
let read_second_line first_line =
  Scanf.sscanf first_line "second line %3d\n"
    (fun second_line_len ->
      (* Printf.fprintf (debugc()) "second line cont %d\n%!" second_line_len; *)
      get_string second_line_len parse_command)


(** Read the first, fixed-length line of a display command and switch
    to {!read_second_line} to process the complete display command.
    This function is the entry point into the display-command parser.
    All command parsing functions set {!Input.current_parser} to
    [read_command_line] when they are finished with their work. This
    way, this function is called again to parse the next command by
    the main parsing loop in {!Input.parse_input}.

    @raise Sys_blocked_io if parsing gets interrupted
    @raise Protocol_error for parsing and protocol errors
*)
let message_start () =
  (* every message starts with a line "second line %03d"
   * where the number gives the bytes in the next line
   *)
  (* Printf.fprintf (debugc()) "message start\n%!"; *)
  try
    get_string 16 read_second_line
  with
    | Scanf.Scan_failure _
    | Failure _ 
    | End_of_file as e 
      ->
      let bt = Printexc.get_backtrace() in
      raise (Protocol_error ("Parse error", Some(e, bt)))


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
    messages.
*)
let error_counter = ref 0


(** Callback for the GTK main loop when input is available on [stdin].
    This is just an exception wrapper around
    {!Input.parse_input_callback}. In case of an escaping exception a
    popup message is displayed and the same message is printed on
    [stderr]. For {!Input.Protocol_error} and [End_of_input] the
    message only contains a backtrace if [debug_mode] in the current
    configuration (see {!Configuration.t} and
    {!Configuration.current_config}) is true. For other exceptions the
    message does always contain the backtrace.
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
	  Printf.bprintf buf "Protocol error!\n%s\nClosing connection." err;
	  prev_exception := prev_e
	| _ ->
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
  message_start_parser := message_start;
  current_parser := message_start;
  setup_input_backup_channel();
  ignore(GMain.Io.add_watch 
	   ~cond:[ `IN ]
	   ~callback:parse_input_callback_ex
	   (GMain.Io.channel_of_descr U.stdin))
