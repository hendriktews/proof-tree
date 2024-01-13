(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2019 Hendrik Tews
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
 *)


(** Reading commands from nonblocking stdin *)

(*****************************************************************************
 *****************************************************************************)
(** {2 Communition Protocol with Proof General}

   The communication protocol between Proof General and Prooftree is
   text based and bidirectional. Proof General sends display messages
   to Prooftree and Prooftree sends user requests and requests for
   additional display messages to Proof General. Prooftree sends
   requests for additional display messages for newly created subgoals
   and for goals that should be updated, because an existential
   variable was instantiated.

    The communication protocol between Proof General and Prooftree is
    split into two parts: The display messages, which are sent from
    Proof General to Prooftree and the request messages, which
    are sent from Prooftree to Proof General.

    {3 Display Messages}

    The protocol for the display messages is designed such that
    Prooftree always knows in advance how many bytes it has to read
    until the end of a message.

    All display messages consist of 
    {ul
    {- a first line of exactly 16 bytes (including the new line) of
    the form "second line 157\n", where the number is the length of
    the second line (including its final newline).}
    {- a second line containing the display command and the length of
    the additional data sections, if the command has data sections.}
    {- the data sections (if any), where the last character of each
    data section is a newline.}
    }

    All data is UTF-8 encoded. 

    Some data sections have a prover specific format. Currently, 
    Prooftree only supports Coq.

    In the following list 
    of commands, ``%d'' stands for a positive integer and ``%s'' for a string
    which contains no white space. ``\{cheated | not-cheated\}'' denotes
    the alternative of either ``cheated'' or ``not-cheated''. An
    integer following the keyword state is a state number. An integer
    following some ``xxx-bytes'' denotes the number of bytes of the next
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
    This version of Prooftree only supports Coq at protocol version 04.
    {%html: <p> %}

    There must always be exectly one configure message, which must be
    the first message. 
    }
    {-  {v current-goals state %d current-sequent %s \
    {cheated | not-cheated} {new-layer | current-layer} proof-name-bytes %d \
    command-bytes %d sequent-text-bytes %d additional-id-bytes %d \
    existential-bytes %d\n\
    <data-proof-name>\n\
    <data-command>\n\
    <data-current-sequent>\n\
    <data-additional-ids>\n\
    <data-existentials>\n v}

    The [current-goals] command tells Prooftree about a new proof 
    state with a new set of open goals. This corresponds to either of
    the following cases:
    {ol
    {- The initial proof state of a newly started proof}
    {- A proof command has been applied to the old current sequent, 
    yielding a new current sequent and possibly additional new 
    open subgoals}
    {- The old current goal has been solved (by some proof command) 
    and the new current sequent is one of the previously spawned 
    subgoals}
    {- A new set of proof-tree root goal nodes is associated with the
    current proof. This happens for instance, when Coq transformes
    open existential variables into proof goals with the command 
    [Grab Existential Variables].}
    }
    
    For case 4 [new-layer] must be given, for case 1 it is optional.
    Otherwise, [current-layer] must be specified. For [current-layer]
    Prooftree decides with its internal state whether case 2 or 3 or 
    possibly 1 applies.
    {%html: <p> %}
    
    For the second and the third case, the set of open goals in the 4th
    data section (additional-ids, see below) does not
    need to represent the total set of all open subgoals, but it must
    contain all newly spawned subgoals.
    {%html: <p> %}
    
    The state number in the [current-goals] command is for undo. It 
    is interpreted as the state that has been reached after processing 
    the current command. 
    [current-sequent %s] denotes the ID of the current sequent. The 
    cheated flag tells Prooftree whether the new proof state was 
    obtained by a cheating command such as [admit]. 
    The data sections are:
    {ol
    {- Full name of the proof}
    {- The proof command that yielded this proof state}
    {- Text of the current sequent}
    {- ID's of additionally open sequents (as space separated 
    list of strings), containing at least all newly spawned subgoals}
    {- Prover specific information about existential variables.}
    }
    
    The second data section is ignored for initial proof states (case 1)
    and new root goal nodes (case 4).
    {%html: <p> %}

    For newly spawned subgoals of this command, Prooftree knows only
    their sequent ID, but cannot display any sequent text. Such sequents
    are called incomplete. They stay incomplete, until their first sequent
    text arrives via an update-sequent command.
    For each newly spawned subgoal, Prooftree sends a show-goal request
    to Proof General. Additionally, also for sequents that contain an
    existential variable that was instantiated by this proof command
    command, Prooftree sends a show-goal command to Proof General in
    order to update the display of the affected sequent. Because Proof
    General and Prooftree progress asynchronously, it may happen, that
    Proof General processes all these show-goal requests only
    substantially later. Each show-goal request will be answered by an
    update-sequent display command (see below) by Proof General.
    }
    {- {v update-sequent state %d sequent %s proof-name-bytes %d \
    sequent-text-bytes %d existential-bytes %d\n\
    <data-proof-name>\n\
    <data-sequent>\n
    <data-existentials>\n v}
    
    The update sequent command updates the text of some 
    known sequent. Such updates are necessary for newly spawned 
    subgoals and for sequents that contain an existantial variable that was
    instantiated. Update sequent commands are always a response to
    a show goal request. Update sequent commands may arrive long after
    the first display command with the same state was processed.
    {%html: <p> %}

    The state number is the (potentially old) state for which the sequent
    text was requested by Prooftree. The sequent ID denotes the 
    sequent to update. The data sections are:

    {ol
    {- Full name of the proof}
    {- new sequent text}
    {- Prover specific information about existential variables.}
    }
    }
    {- {v switch-goal state %d sequent %s proof-name-bytes %d\n
    <data-proof-name>\n v}
    
    Switch goal tells Prooftree that the current goal has changed 
    without changing or solving the old current goal.
    {%html: <p> %}

    The state number is for undo and the only data section is:
    {ol
    {- Full name of the proof}
    }
    }
    {- {v branch-finished state %d {cheated | not-cheated} \
    proof-name-bytes %d command-bytes %d existential-bytes %d\n\
    <data-proof-name>\n\
    <data-command>\n\
    <data-existentials>\n v}
    
    [branch-finished] tells Prooftree the last proof command that 
    closed the current branch. If there are still open subgoals, the 
    proof will hopefully continue with one of them, which is not yet 
    known. The cheated flag tells Prooftree
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
    completed and will further not be updated. After sending
    [proof-complete] Proof General blocks processing the queue region 
    until it receives a [confirm-proof-complete] request message. This
    way, Proof General will process all [show-goal] request message
    necessary for this proof before disabling the dependent evar line
    in Coq and proceeding with the queue region. The [show-goal] messages
    for incomplete sequents have been sent out when [proof-complete] is
    processed here, however the corresponding [update-sequent] might
    arrive later. Therefore Prooftree might send out [show-goal] messages
    for evar instantiations after receiving [proof-complete] and after
    receiving all [update-sequent] messages for all incomplete sequents.
    {%html: <p> %}

    The last open branch should have been closed via [branch-finished]
    before [proof-complete] arrives, otherwise the proof will be in a
    strange state.
    {%html: <p> %}

    The only data section of [proof-complete] is:
    {ol
    {- Full name of the proof}
    }
    }
    {- {v undo-to state %d\n v}
    
    The state number here is not for undo, it is the undo-state.
    Undo tells Prooftree to change the display to the state before 
    the first command with a state strictly greater than [undo-state]
    has been processed.
    }
    {- {v quit-proof proof-name-bytes %d\n\
    <data-proof-name>\n v}
    
    This command tells Prooftree that the user stoped the proof-tree
    display in Proof General. Prooftree will close the main window for
    that proof.
    Cloned windows are not closed.
    The only data section is:
    {ol
    {- Full name of the proof whoose window should be delected}
    }
    }
    }

    {3 Request Messages}

    The request messages are sent from Prooftree to Proof General to
    request updates of sequents and certain reactions for user
    interactions. The protocol relies on the fact that [PIPE_BUF] bytes
    (512 required by POSIX, 4096 on Linux) are transmitted atomically
    in a pipe. On the receiver side, in Proof General, there are no
    precautions against not completely transferred first lines
    (including both newlines) of request messages. 

    All request
    messages are preceeded with a newline and the string 
    [emacs exec:], followed by a space, and terminated with an additional
    newline for easy recognition in Proof
    General.

    The variable parts of the request messages are as follows.
    {ul
    {- {v show-goal "%s" at %d for "%s" v}

    The first string is the goal id, the number after [at] is a state and
    the last string is a name of a proof.
    {%html: <p> %}

    This command requests Proof General to send an [update-sequent] display
    message for the specified goal in the specified state. All data, the goal
    id, the state and the proof name, actually makes a round trip to Proof
    General and comes back in the [update-sequent] command. Note that the
    state might be an arbitrary previous state of the current proof,
    occurring in a preceding [current-goals] command.
    {%html: <p> %}

    Prooftree sends a [show-goal] command for each newly spawned subgoal
    from a [current-goals] command and for each goal that contains an
    existential variable that was reported to have gotten instantiated
    in a [current-goals] command. For the latter case, it may happen that
    Prooftree finds out about the existential only because of an
    [update-sequent] command, such that it possibly sends [show-goal]
    much later than receiving the [current-goals] command that reported the
    instantiation.
    }
    {- {v stop-displaying v}
    
    Prooftree sends this message to Proof General when the user closed
    the proof-tree display of a proof currently under development.
    Proof General then stops sending display commands for that proof.
    }
    {- {v undo %d v}

    Prooftree sends the undo message, when the user selected an undo
    for a certain sequent from the context menu. The integer is the
    undo state number of the proof command child node of the selected
    sequent. 
    }
    {- {v insert-proof-script %d\n<script data>\n v}

    Prooftree sends this message when the user selected the Insert
    command or Insert subproof items from the context menu. The
    integer is the length of [<script data>] without the enclosing
    newlines.
    }
    {- {v confirm-proof-complete "%s" v}

    This message confirms that processing the named proof is complete and
    that all [show-goal] request messages for this proof have been sent
    before. This message is required after receiving a [proof-complete]
    display message.
    }
    }
*)

(*****************************************************************************
 *****************************************************************************)

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
open Evar_types

(**/**)
module U = Unix
(**/**)


(** {2 Module Documentation} *)

(** Version number of the communication protocol described and
    implemented by this module.
*)
(* update documentation for configure message when changed *)
let protocol_version = 4


(** {3 General parsing utilities and parser state} *)


(** Exception raised if Prooftree encounters an unknown or malformed command.
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
  ref(fun _ -> ([], []) : string -> (evar_info list * string list))

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
  else begin
    (match !input_backup_oc with
      | None -> ()
      | Some oc -> 
	close_out oc;
	input_backup_oc := None;
	input_backup_filename := None;
    );
    if !current_config.copy_input
    then begin
      (try
	 input_backup_oc := Some(open_out !current_config.copy_input_file);
       with
	 | Sys_error msg -> raise (Log_input_file_error msg)
      );
      input_backup_filename := Some !current_config.copy_input_file;
    end else begin
      input_backup_oc := None;
      input_backup_filename := None;
    end
  end


(** Input function for reading from the input channel. To make the
    input backup feature work (see option [-tee]) input must always be
    read with this function. Arguments are the same as for {xref
    stdlib val Stdlib.input}, [local_input buf start len] reads at
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

    @raise Sys_blocked_io when no more input is available currently.
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
    fills buffer [s] and continues parsing with [cont]. This
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
    continuation_fn (Bytes.to_string s)
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
  let s = Bytes.create len in
  get_string_cont s 0 len continuation_fn ()


(******************************************************************************
 ******************************************************************************
 * Build Coq evar parser
 *)

(** {3 Build Coq existential variable info parser} *)

(** Parse Coq existential variable information. Returns a pair. The
   first element is a list of {!Evar_types.evar_info} describing open
   evars with their external name and instantiated ones with their
   dependencies. The second element is the list of internal evar names
   occuring in the current goal. Uses an OCamllex and OCamlyacc
   generated parser internally. All parsing exceptions are cought and
   converted into warning messages. *)
let coq_evar_parser (input_string : string)
    : (evar_info list * string list) =
  try
    Coq_evar_parser.coq_evar_info Coq_evar_lexer.evar_token
      (Lexing.from_string input_string)
  with
    | e ->
       if !current_config.debug_mode then
         Printf.eprintf
           "Coq evar parser error on \"%s\"\nParser aborts with exception %s\n%!"
           input_string (Printexc.to_string e);
       run_message_dialog
         "Coq evar parser error.\nExistential info might be wrong."
         `WARNING;
       ([], [])

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
      parse_existential_info := coq_evar_parser
    (* | "HOL Light" -> () *)
    | _ -> 
      raise (Protocol_error ("Unknown proof assistant " ^ proof_assistant,
			     None))
  );
  if protocol_version <> pg_protocol_version then
    raise (Protocol_error 
	     ((Printf.sprintf
		 ("Communication protocol mismatch.\n"
		  ^^ "Proof General uses version %02d,\n"
		  ^^ "but this version of Prooftree supports only version %02d.")
		 pg_protocol_version protocol_version),
	      None));
  configure_message_received := true


(** Parse the configure message and process it. *)
let parse_configure com_buf =
  Scanf.bscanf com_buf 
    " for \"%s@\" and protocol version %d" configure_prooftree

(******************************************************************************
 ******************************************************************************
 * current-goals state %d current-sequent %s {cheated | not-cheated} \
 * {new-layer | current-layer}
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
    @param layer_string either "new-layer" of "current-layer" from the
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
    layer_string proof_name proof_command current_sequent_text 
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
  let layer_flag = match layer_string with
    | "new-layer" -> true
    | "current-layer" -> false
    | _ ->
      raise(Protocol_error
	      ("Parse error in current-goals command. " ^
		  "Expected \"new-layer\" or \"current-layer\" as 7th word.",
	       None))
  in
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  let current_sequent_text = chop_final_newlines current_sequent_text in
  let additional_ids_string = chop_final_newlines additional_ids_string in
  let additional_ids = string_split ' ' additional_ids_string in
  let (evar_info, current_evar_names) =
    !parse_existential_info existentials_string in
  Proof_tree.process_current_goals state proof_name proof_command cheated_flag
    layer_flag current_sequent_id current_sequent_text additional_ids 
    evar_info current_evar_names;
  current_parser := !message_start_parser

    

(** Start parsing of the [current-goals] command. Extracts elements and 
    string length' from the [Scanf] parsing buffer argument and reads
    all the necessary strings from the input channel. When reading finished 
    {!Input.parse_current_goals_finish} is called.
*)
let parse_current_goals com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf 
    (" state %d current-sequent %s %s %s proof-name-bytes %d "
     ^^ "command-bytes %d sequent-text-bytes %d "
     ^^ "additional-id-bytes %d existential-bytes %d")
    (fun state current_sequent_id cheated_string layer_string 
      proof_name_bytes command_bytes sequent_text_bytes additional_id_bytes
      existential_bytes ->
        (*
         * Printf.fprintf (debugc())
	 * 	       ("PCGs state %d current-sequent %s cheated %s layer %s "
	 * 		^^ "proof-name-len %d command-len %d sequent-len %d "
	 * 		^^ "id-len %d existential-len %d\n%!")
	 * 	       state current_sequent_id cheated_string layer_string
	 * 	       proof_name_bytes command_bytes sequent_text_bytes
	 * 	       additional_id_bytes existential_bytes;
         *)
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
			      cheated_string layer_string
			      proof_name proof_command current_sequent_text
			      additional_ids_string existentials_string))))))



(******************************************************************************
 * update-sequent state %d sequent %s proof-name-bytes %d \
 * sequent-text-bytes %d existential-bytes %d\n
 * <data-proof-name>\n
 * <data-sequent>\n
 * <data-existentials>\n
 *)

(** {3 Update-sequent command parser} *)


(** Finish parsing of the [update-sequent] command and call
    {!Proof_tree.update_sequent} to update the sequent. The 
    arguments are as follows:

    @param state state number
    @param sequent_id ID of sequent to update
    @param proof_name full proof name (as raw data section string)
    @param sequent_text new sequent text (as raw data section string)
    @param existentials_string prover specific information about 
    existentials
*)
let parse_update_sequent_finish state sequent_id proof_name sequent_text
      existentials_string =
  let proof_name = chop_final_newlines proof_name in
  let sequent_text = chop_final_newlines sequent_text in
  let (evar_info, current_evar_names) =
    !parse_existential_info existentials_string in
  Proof_tree.update_sequent state proof_name sequent_id sequent_text
    evar_info current_evar_names;
  current_parser := !message_start_parser



(** Parse and process a [update-sequent] command. Extracts the state and
    the data section length' from the first command line in the [Scanf]
    parsing buffer argument, reads the data sections and finally call
    {!Input.parse_update_sequent_finish}.
*)
let parse_update_sequent com_buf =
  check_if_configured ();
  Scanf.bscanf com_buf
    (" state %d sequent %s proof-name-bytes %d sequent-text-bytes %d "
     ^^ "existential-bytes %d")
    (fun state sequent_id proof_name_bytes sequent_text_bytes
         existential_bytes ->
      get_string proof_name_bytes
	(fun proof_name ->
	  get_string sequent_text_bytes
	    (fun sequent_text ->
              get_string existential_bytes
                (fun existentials_string ->
	             parse_update_sequent_finish state sequent_id
		     proof_name sequent_text existentials_string))))



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
 * branch-finished state %d {cheated | not-cheated} \
 * proof-name-bytes %d command-bytes %d existential-bytes %d\n\
 * <data-proof-name>\n\
 * <data-command>\n\
 * <data-existentials>\n
*)

(** {3 Branch-finished command parser} *)


(** Finish parsing of the [branch-finished] command and process it
    with {!Proof_tree.process_branch_finished}. The arguments are

    @param state state number
    @param cheated_string either "cheated" or "not-cheated"
    @param proof_name full proof name (as raw data section string)
    @param proof_command last proof command (as raw data section string)
    @param existentials_string prover specific data about existentials
*)
let parse_branch_finished_finish 
    state cheated_string proof_name proof_command existentials_string =
  let cheated_flag = match cheated_string with
    | "not-cheated" -> false
    | "cheated" -> true
    | _ -> 
      raise(Protocol_error
	      ("Parse error in branch-finished command. " ^
		  "Expected \"cheated\" or \"not-cheated\" as 4th word.",
	       None))
  in
  let proof_name = chop_final_newlines proof_name in
  let proof_command = chop_final_newlines proof_command in
  let (evar_info, current_evar_names) =
    !parse_existential_info existentials_string in
  Proof_tree.process_branch_finished 
    state proof_name proof_command cheated_flag evar_info current_evar_names;
  current_parser := !message_start_parser


(** Parse and process a [proof-finished] command. Extracts the
    necessary information from the first command line in the [Scanf]
    parsing buffer argument, reads the data section and finally calls
    {!Input.parse_branch_finished_finish}.
*)
let parse_branch_finished com_buf =
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
		  parse_branch_finished_finish 
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
      | "branch-finished" -> parse_branch_finished com_buf
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
    [message_start] when they are finished with their work. This
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
  get_string 16 read_second_line


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
    [stdin]. Puts [stdin] into non-blocking mode. If
    [setup_input_backup_channel] raises an [Sys_error] (because of an
    invalid file name), this function is called a second time with
    input logging disabled.
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
