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
 * $Id: configuration.ml,v 1.34 2012/03/06 14:57:45 tews Exp $
 *)


(** Prooftree Configuration and the Configuration Dialog *)

open Util
open Gtk_ext

(**/**)
module U = Unix
(**/**)


(*****************************************************************************
 *****************************************************************************)
(** {2 Configuration record and global variables} *)


(** Hardwired location of the user-specific configuration file. *)
let config_file_location = 
  Filename.concat
    (Sys.getenv "HOME")
    ".prooftree"

(** Configuration record. For simplicity the user specific
    configuration file is (mostly) a marshaled configuration record.
    In order to be independent of Gdk marshaling, the configuration
    record consists only of pure OCaml values. Fonts and colors are
    therefore not accessed via the configuration record, but via there
    own references (of some suitable Gdk type). These references must,
    of course, be kept in sync with the current configuration. All
    other configurable values are accessed through the current
    configuration record, which is stored in {!current_config}.
*)
(* IMPORTANT: INCREASE config_file_version BELOW WHEN CHANGING THIS RECORD *)
type t = {
  turnstile_radius : int;
  (** Radius (in pixel) of the circle around the turnstile symbol for
      the current node. Used also as kind of circular bounding box of
      the turnstile symbol.
  *)

  turnstile_left_bar_x_offset : int;
  (** X-offset of the vertical bar of the turnstile symbol. *)

  turnstile_left_bar_y_offset : int;
  (** Y-offset of the upper and lower end of the vertical bar of the
      turnstile symbol (with respect to the centre of the vertical
      bar).
  *)

  turnstile_horiz_bar_x_offset : int;
  (** Length of the horizontal bar of the turnstile symbol. *)
  
  turnstile_line_width : int;
  (** Line width of all lines (including the turnstile symbol). *)

  turnstile_number_x_offset : int;
  (** X-offset (with respect to the centre of the turnstile symbol) at
      which the number of the external sequent window is printed, if
      there is any.
  *)

  proof_command_length : int;
  (** Maximal number of characters that are displayed for a proof
      command in the proof-tree display.
  *)

  subtree_sep : int;
  (** Additional space added between two adjacent subtrees. (More
      precisely, this value is added to the width of every node in the
      proof-tree display.)
  *)

  line_sep : int;
  (** Space left between nodes and connecting lines. *)

  level_distance : int;
  (** Vertical distance between two levels of the proof tree. *)

  button_1_drag_acceleration : float;
  (** Acceleration multiplier for dragging the proof-tree display
      inside its viewport. Positive values move the viewport (i.e.,
      the tree underneath moves in the opposite direction of the
      mouse), negative values move the tree (i.e., the tree underneath
      moves in the same direction as the mouse).
  *)

  proof_tree_font : string;
  (** Font description (as for {!GPango.font_description}) for the
      text inside the proof-tree display.
  *)

  sequent_font : string;
  (** Font description (as for {!GPango.font_description}) for the
      text in the sequent display and in the additional node windows.
  *)

  current_color : (int * int * int);
  (** The color for the current branch, as 16-bit RGB value. *)

  cheated_color : (int * int * int);
  (** The color for branches that have been finished with a cheating
      command, as 16-bit RGB value 
  *)

  proved_complete_color : (int * int * int);
  (** The color for branches that have been proved and which depend on no
      non-instantiated existential variables, as 16-bit RGB value.
  *)

  proved_incomplete_color : (int * int * int);
  (** The color for branches that have been proved and that have
      non-instantiated existential variables, as 16-bit RGB value.
  *)

  proved_partial_color : (int * int * int);
  (** The color for branches that have been proved and whose own 
      existential variables are all instantiated, but where the 
      instantiations depend on some not-yet instantiated existential 
      variables. The value is the 16-bit RGB triple.
  *)

  (* 
   * mark_subtree_color : (int * int * int);
   * (\** The color for marked subtrees, as 16 bit RGB value. *\)
   *)

  existential_create_color : (int * int * int);
  (** The color for marking nodes that introduce a given existential
      variable, as 16 bit RGB value.
  *)

  existential_instantiate_color : (int * int * int);
  (** The color for marking nodes that intantiate a given existential
      variable, as 16 bit RGB value. 
  *)

  display_doc_tooltips : bool;
  (** Whether to display documentation/help tool-tips. *)
  
  display_turnstile_tooltips : bool;
  (** Whether to display complete sequents as tool-tips over sequent
      symbols.
  *)

  display_command_tooltips : bool;
  (** Whether to display complete proof commands as tool-tips over
      proof commands.
  *)

  default_width_proof_tree_window : int;
  (** Default width of the proof-tree window, used if there was no
      [-geometry] option.
  *)

  default_height_proof_tree_window : int;
  (** Default heigth of the proof-tree window, used if there was no
      [-geometry] option.
  *)

  internal_sequent_window_lines : int;
  (** Number of text lines in the internal sequent window. If [0] the
      internal sequent window is hidden.
  *)

  node_window_max_lines : int;
  (** Maximal number of text lines in external node windows. *)

  debug_mode : bool;
  (** Print more exception backtraces for internal errors, if true. *)

  copy_input : bool;
  (** Write all read input into the file [copy_input_file], if true. *)

  copy_input_file : string;
  (** File to write read input to, if [copy_input] is true. *)
}

(** Set the fields [turnstile_left_bar_x_offset],
    [turnstile_left_bar_y_offset] and [turnstile_horiz_bar_x_offset]
    as function of the field [turnstile_radius]. Set
    [turnstile_number_x_offset] as function of [turnstile_line_width]
    (see {!t}).
*)
let update_sizes config =
  let radius = config.turnstile_radius in
  { config with 
    turnstile_left_bar_x_offset = 
      int_of_float(-0.23 *. (float_of_int radius) +. 0.5);
    turnstile_left_bar_y_offset =
      int_of_float(0.65 *. (float_of_int radius) +. 0.5);
    turnstile_horiz_bar_x_offset =
      int_of_float(0.7 *. (float_of_int radius) +. 0.5);
    turnstile_number_x_offset = -(config.turnstile_line_width + 1);
  }

(** Create the default, builtin configuration record. *)
let default_configuration = 
  let radius = 10 in
  let blue = GDraw.color (`NAME "blue") in
  let red = GDraw.color (`NAME "red") in
  let c = {
    turnstile_radius = radius;
    turnstile_line_width = 2;
    proof_command_length = 15;
    subtree_sep = 5;
    line_sep = 3;
    level_distance = 38;

    turnstile_left_bar_x_offset = 0;
    turnstile_left_bar_y_offset = 0;
    turnstile_horiz_bar_x_offset = 0;
    turnstile_number_x_offset = 0;

    button_1_drag_acceleration = 4.0;

    proof_tree_font = "Sans 8";
    sequent_font = "Sans 8";

    current_color = 
      (Gdk.Color.red blue, Gdk.Color.green blue, Gdk.Color.blue blue);
    cheated_color = 
      (Gdk.Color.red red, Gdk.Color.green red, Gdk.Color.blue red);
    proved_complete_color = (19 * 255, 197 * 256, 19 * 255);
    proved_partial_color = (100 * 256, 114 * 256, 0 * 256);
    proved_incomplete_color = (26 * 255, 226 * 256, 216 * 256);
    (* mark_subtree_color = (0,0,0); *)
    existential_create_color = (255 * 256, 0xF5 * 256, 0x8F * 256);
    existential_instantiate_color = (255 * 256, 0xB6 * 256, 0x6D * 256);

    display_doc_tooltips = true;
    display_turnstile_tooltips = true;
    display_command_tooltips = true;

    default_width_proof_tree_window = 400;
    default_height_proof_tree_window = 400;

    internal_sequent_window_lines = 1;
    node_window_max_lines = 35;

    debug_mode = false;
    copy_input = false;
    copy_input_file = "/tmp/prooftree.log";
  }
  in
  update_sizes c


(** Reference of the internal configuration record. Most configuration
    values are accessed through this reference. For fonts and colors
    there are separate references, which are always updated, when the
    configuration changes.
*)
let current_config = ref default_configuration


(** Font description for the text inside the proof-tree display, as
    value of {!GPango.font_description} type. Should always be in sync
    with the [proof_tree_font] field of {!current_config}.
*)
let proof_tree_font_desc = 
  ref(GPango.font_description default_configuration.proof_tree_font)


(** Font description for the text in the sequent display and in the
    additional node windows, as value of {!GPango.font_description}
    type. Should always be in sync with the [sequent_font] field
    of {!current_config}.
*)
let sequent_font_desc = 
  ref(GPango.font_description default_configuration.sequent_font)


(** Color for the current branch, as {!Gdk.color}. Should always be in
    sync with the [current_color] field of {!current_config}.
*)
let current_gdk_color =
  ref(GDraw.color (`RGB default_configuration.current_color))


(** Color for branches that have been finished with a cheating
    command, as {!Gdk.color}. Should always be in sync with the
    [cheated_color] field of {!current_config}.
*)
let cheated_gdk_color =
  ref(GDraw.color (`RGB default_configuration.cheated_color))


(** Color for branches that have been proved and which have no
    non-instantiated esistential variables, as {!Gdk.color}. Should
    always be in sync with the [proved_complete_color] field of
    {!current_config}.
*)
let proved_complete_gdk_color = 
  ref(GDraw.color (`RGB default_configuration.proved_complete_color))


(** Color for branches that have been proved and that have
    non-instantiated existential variables as {!Gdk.color}. Should
    always be in sync with the [proved_incomplete_color] field of
    {!current_config}.
*)
let proved_incomplete_gdk_color = 
  ref(GDraw.color (`RGB default_configuration.proved_incomplete_color))


(** Color for branches that have been proved and whose own existential
    variables are all instantiated, but where the instantiations
    depend on some not-yet instantiated existential variables. The
    value is given as {!Gdk.color} and should always be in sync with
    the [proved_partial_color] field of {!current_config}.
*)
let proved_partial_gdk_color =
  ref(GDraw.color (`RGB default_configuration.proved_partial_color))


(* 
 * (\** Color for marked subtrees, as {!Gdk.color}. Should always be in
 *     sync with the {!mark_subtree_color} field of {!current_config}.
 * *\)
 * let mark_subtree_gdk_color =
 *   ref(GDraw.color (`RGB default_configuration.mark_subtree_color))
 *)


(** Color for marking nodes that introduce a given existential
    variable, as {!Gdk.color}. Should always be in sync with the
    [existential_create_color] field of {!current_config}.
*)
let existential_create_gdk_color =
  ref(GDraw.color (`RGB default_configuration.existential_create_color))

(** Color for marking nodes that instantiate a given existential
    variable, as {!Gdk.color}. Should always be in sync with the
    [existential_instantiate_color] field of {!current_config}.
*)
let existential_instantiate_gdk_color =
  ref(GDraw.color (`RGB default_configuration.existential_instantiate_color))


(** Update the references for fonts and colors after the current
    configuration has been changed.
*)
let update_font_and_color () =
  proof_tree_font_desc :=
    GPango.font_description !current_config.proof_tree_font;
  sequent_font_desc :=
    GPango.font_description !current_config.sequent_font;
  current_gdk_color :=
    GDraw.color (`RGB !current_config.current_color);
  cheated_gdk_color :=
    GDraw.color (`RGB !current_config.cheated_color);
  proved_complete_gdk_color :=
    GDraw.color (`RGB !current_config.proved_complete_color);
  proved_incomplete_gdk_color :=
    GDraw.color (`RGB !current_config.proved_incomplete_color);
  proved_partial_gdk_color :=
    GDraw.color (`RGB !current_config.proved_partial_color);
  (* 
   * mark_subtree_gdk_color :=
   *   GDraw.color (`RGB !current_config.mark_subtree_color);
   *)
  existential_create_gdk_color :=
    GDraw.color (`RGB !current_config.existential_create_color);
  existential_instantiate_gdk_color :=
    GDraw.color (`RGB !current_config.existential_instantiate_color)


(** This function reference solves the recursive module dependency
    between modules {!Proof_tree}, {!Input} and this module. It is
    filled with {!Main.configuration_updated} when [Main] is
    initialized.
*)
let configuration_updated_callback = ref (fun () -> ())


(** [update_configuration c] does all the necessary actions to make
    [c] the current configuration. It stores [c] in {!current_config},
    updates the references for fonts and colors and calls all
    [configuration_updated] functions/methods.
*)
let update_configuration c =
  current_config := c;
  update_font_and_color ();
  !configuration_updated_callback ()


(** Reference for the argument of the [-geometry] option. *)
let geometry_string = ref ""


(*****************************************************************************
*****************************************************************************)
(** {2 Save / Restore configuration records} 
    
    A configuration file consists of an ASCII header (followed by a
    newline) and a marshaled configuration record (of type {!t}).
    Because of the header one can easily identify the file by opening
    it in any editor. The header contains also a version field, which
    changes whenever the type of the marshaled value changes.
*)

(** Common header of all configuration files. *)
let config_file_header_start = "Prooftree configuration file version "

(** Version specific header of the current config file version. *)
let config_file_version = "03"

(** The complete ASCII header of configuration files. *)
let config_file_header = config_file_header_start ^ config_file_version ^ "\n"

(** [write_config_file file c] writes a config file at [file],
    containing the configuration record [c].
*)
let write_config_file file_name (config : t) =
  let oc = open_out_bin file_name in
  output_string oc config_file_header;
  Marshal.to_channel oc config [];
  close_out oc

(** Read a configuration file at the specified location. Raises
    [Sys_error] if the file is not present or not readable. Raises
    [Failure] if there is no configuration file or if the file has an
    incompatible version. Return the read configuration file on success.
*)
let read_config_file file_name : t =
  let header_len = String.length config_file_header in
  let header = String.create header_len in
  let ic = open_in_bin file_name in
  really_input ic header 0 header_len;
  if header = config_file_header 
  then begin
    let c = (Marshal.from_channel ic : t) in 
    close_in ic;
    c
  end
  else if string_starts header config_file_header_start
  then raise(Failure "Incompatible configuration file version")
  else raise(Failure "Invalid configuration file")

(** Try to load the configuration file at {!config_file_location},
    ignoring all errors. If a valid configuration file is found, the
    current configuration is updated. Used during start-up.
*)
let try_load_config_file () =
  let copt =
    try
      Some(read_config_file config_file_location)
    with
      | _ -> None
  in
  match copt with
    | None -> ()
    | Some c -> update_configuration c


(*****************************************************************************
*****************************************************************************)
(** {2 Configuration Dialog} *)


(** Reference to ensure that at most one configuration window does
    exist. 
*)
let config_window = ref None


(** Class for managing configuration windows. Objects are created when
    the widget tree is completely constructed. Contains the necessary
    state and methods to handle all callbacks. The callbacks must be
    set up by the function that creates objects. 

    Arguments are
    - top_window		{!GWindow.window} of the top-level widget
    - line_width_spinner 	{!GEdit.spin_button} for line width
    - turnstile_size_spinner 	{!GEdit.spin_button} for turnstile size
    - line_sep_spinner 		{!GEdit.spin_button} for line gap
    - subtree_sep_spinner 	{!GEdit.spin_button} for node padding
    - command_length_spinner 	{!GEdit.spin_button} for command length
    - level_dist_spinner 	{!GEdit.spin_button} for vertical distance
    - tree_font_button		{!GButton.font_button} for proof tree font
    - sequent_font_button	{!GButton.font_button} for sequent window font
    - current_color_button	{!GButton.color_button} for current color
    - cheated_color_button	{!GButton.color_button} for cheated color
    - proved_complete_color_button   {!GButton.color_button} for complete color
    - proved_incomplete_color_button {!GButton.color_button} for incomplete color
    - proved_partial_color_button    {!GButton.color_button} for partial color
    - ext_create_color_button     {!GButton.color_button} for create exist.
    - ext_inst_color_button       {!GButton.color_button} for instant. exist.
    - drag_accel_spinner 	  {!GEdit.spin_button} for drac acceleration
    - doc_tooltip_check_box	  {!GButton.toggle_button} for the help 
                                  tool-tips check bock
    - turnstile_tooltip_check_box {!GButton.toggle_button} for the turnstile
                                  tool-tips check bock
    - command_tooltip_check_box	  {!GButton.toggle_button} for the command
                                  tool-tips check bock
    - default_size_width_spinner  {!GEdit.spin_button} for default window 
                                  size width
    - default_size_height_spinner {!GEdit.spin_button} for default window 
                                  size height
    - internal_seq_lines_spinner  {!GEdit.spin_button} for lines in the 
                                  internal sequent window
    - external_node_lines_spinner {!GEdit.spin_button} for lines in external 
                                  node windows
    - debug_check_box		{!GButton.toggle_button} for the 
                                more-debug-info check box
    - tee_file_box_check_box 	{!GButton.toggle_button} for log-input check box
    - tee_file_name_label	{!GMisc.label} of the log-file label
    - tee_file_name_entry	{!GEdit.entry} of the log-file text entry
    - tee_file_name_button	{!GButton.button} of the log-file button that 
                                starts the file selection dialog
    - tooltip_misc_objects	list of {!GObj.misc_ops} of config dialog
                                elements that have a tool-tip to switch on and
                                off
*)
class config_window 
  top_window
  line_width_spinner
  turnstile_size_spinner
  line_sep_spinner
  subtree_sep_spinner
  command_length_spinner
  level_dist_spinner
  tree_font_button
  sequent_font_button
  current_color_button
  cheated_color_button
  proved_complete_color_button
  proved_incomplete_color_button
  proved_partial_color_button
  (* mark_subtree_color_button *)
  ext_create_color_button
  ext_inst_color_button
  drag_accel_spinner
  doc_tooltip_check_box
  turnstile_tooltip_check_box
  command_tooltip_check_box
  default_size_width_spinner default_size_height_spinner
  internal_seq_lines_spinner
  external_node_lines_spinner
  debug_check_box
  tee_file_box_check_box 
  tee_file_name_label tee_file_name_entry tee_file_name_button
  tooltip_misc_objects  
  =
object (self)

  (** {!GData.adjustment} of the line-width spin button. *)
  val line_width_adjustment = line_width_spinner#adjustment

  (** {!GData.adjustment} of the turnstile-size spin button. *)
  val turnstile_size_adjustment = turnstile_size_spinner#adjustment

  (** {!GData.adjustment} of the line-gap spin button. *)
  val line_sep_adjustment = line_sep_spinner#adjustment

  (** {!GData.adjustment} of the node-padding spin button. *)
  val subtree_sep_adjustment = subtree_sep_spinner#adjustment

  (** {!GData.adjustment} of the command-length spin button. *)
  val command_length_adjustment = command_length_spinner#adjustment

  (** {!GData.adjustment} of the vertical-distance spin button. *)
  val level_dist_adjustment = level_dist_spinner#adjustment

  (** {!GData.adjustment} of the drag-acceleration spin button. *)
  val drag_accel_adjustment = drag_accel_spinner#adjustment

  (** {!GData.adjustment} of the default-window-size-width spin button. *)
  val default_size_width_adjustment = default_size_width_spinner#adjustment

  (** {!GData.adjustment} of the default-window-size-height spin button. *)
  val default_size_height_adjustment = default_size_height_spinner#adjustment

  (** {!GData.adjustment} of the spin button for the number of lines
      in the internal sequent window. 
  *)
  val internal_seq_lines_adjustment = internal_seq_lines_spinner#adjustment

  (** {!GData.adjustment} of the spin button for the number of lines
      in external node windows. 
  *)
  val external_node_lines_adjustment = external_node_lines_spinner#adjustment

  (** Make this configuration dialog visible. *)
  method present = top_window#present()

  (** [set_configuration c] changes spinners and buttons to show the
      configuration of the configuration record [c].
  *)
  method set_configuration conf =
    line_width_adjustment#set_value (float_of_int conf.turnstile_line_width);
    turnstile_size_adjustment#set_value (float_of_int conf.turnstile_radius);
    subtree_sep_adjustment#set_value (float_of_int conf.subtree_sep);
    line_sep_adjustment#set_value (float_of_int conf.line_sep);
    command_length_adjustment#set_value (float_of_int conf.proof_command_length);
    level_dist_adjustment#set_value (float_of_int conf.level_distance);
    tree_font_button#set_font_name conf.proof_tree_font;
    sequent_font_button#set_font_name conf.sequent_font;
    current_color_button#set_color (GDraw.color (`RGB conf.current_color));
    cheated_color_button#set_color (GDraw.color (`RGB conf.cheated_color));
    proved_complete_color_button#set_color
      (GDraw.color (`RGB conf.proved_complete_color));
    proved_incomplete_color_button#set_color
      (GDraw.color (`RGB conf.proved_incomplete_color));
    proved_partial_color_button#set_color
      (GDraw.color (`RGB conf.proved_partial_color));
    (* 
     * mark_subtree_color_button#set_color
     *   (GDraw.color (`RGB conf.mark_subtree_color));
     *)
    ext_create_color_button#set_color
      (GDraw.color (`RGB conf.existential_create_color));
    ext_inst_color_button#set_color
      (GDraw.color (`RGB conf.existential_instantiate_color));
    drag_accel_adjustment#set_value conf.button_1_drag_acceleration;
    doc_tooltip_check_box#set_active conf.display_doc_tooltips;
    turnstile_tooltip_check_box#set_active conf.display_turnstile_tooltips;
    command_tooltip_check_box#set_active conf.display_command_tooltips;
    default_size_width_adjustment#set_value
      (float_of_int conf.default_width_proof_tree_window);
    default_size_height_adjustment#set_value
      (float_of_int conf.default_height_proof_tree_window);
    internal_seq_lines_adjustment#set_value
      (float_of_int conf.internal_sequent_window_lines);
    external_node_lines_adjustment#set_value
      (float_of_int conf.node_window_max_lines);
    debug_check_box#set_active conf.debug_mode;
    tee_file_box_check_box#set_active conf.copy_input;
    tee_file_name_entry#set_text conf.copy_input_file;
    ()

  (** Change spinners and buttons to show the compile-time default
      configuration. 
  *)
  method reset_to_default () =
    self#set_configuration default_configuration

  (** Switch the help/documentation tool-tips on or off, according to
      the state of the help-tool-tips check box.
  *)
  method toggle_tooltips () =
    let flag = doc_tooltip_check_box#active in
    List.iter (fun misc -> misc#set_has_tooltip flag) tooltip_misc_objects;
    ()

  (** Change the visibility of the tee-file elements, according to the
      state of the log-input check box.
  *)
  method tee_file_toggle () =
    let flag = tee_file_box_check_box#active in
    tee_file_name_label#misc#set_sensitive flag;
    tee_file_name_entry#misc#set_sensitive flag;
    tee_file_name_button#misc#set_sensitive flag;
    ()

  (** Start and manage the modal file selection dialog for the
      log-file button. If the user makes a selection, the log-file
      text entry is updated.
  *)
  method tee_file_button_click () =
    let file_chooser = GWindow.file_chooser_dialog 
      ~action:`SAVE
      ~parent:top_window
      ~destroy_with_parent:true
      ~title:"Prooftree log file selection"
      ~focus_on_map:true
      ~modal:true ()
    in
    file_chooser#add_select_button "Select" `SELECT;
    file_chooser#add_button "Cancel" `CANCEL;
    ignore(file_chooser#set_current_folder 
	     (Filename.dirname tee_file_name_entry#text));
    (match file_chooser#run() with
      | `SELECT -> 
	(match file_chooser#filename with
	  | None -> ()
	  | Some file -> tee_file_name_entry#set_text file
	)
      | `CANCEL
      | `DELETE_EVENT -> ()
    );
    file_chooser#destroy();
    ()

  (** Create a new configuration record with the current values of the
      spinners and buttons of this configuration dialog.
  *)
  method private extract_configuration =
    let round_to_int f = int_of_float(f +. 0.5) in
    let c = {
      turnstile_radius = round_to_int turnstile_size_adjustment#value;
      turnstile_line_width = round_to_int line_width_adjustment#value;
      proof_command_length = round_to_int command_length_adjustment#value;
      subtree_sep = round_to_int subtree_sep_adjustment#value;
      line_sep = round_to_int line_sep_adjustment#value;
      level_distance = round_to_int level_dist_adjustment#value;

      turnstile_left_bar_x_offset = 0;
      turnstile_left_bar_y_offset = 0;
      turnstile_horiz_bar_x_offset = 0;
      turnstile_number_x_offset = 0;

      button_1_drag_acceleration = drag_accel_adjustment#value;

      proof_tree_font = tree_font_button#font_name;
      sequent_font = sequent_font_button#font_name;

      current_color = (let c = current_color_button#color in
		       (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
      cheated_color = (let c = cheated_color_button#color in
		       (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
      proved_complete_color = 
	(let c = proved_complete_color_button#color in
	 (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
      proved_incomplete_color = 
	(let c = proved_incomplete_color_button#color in
	 (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
      proved_partial_color =
	(let c = proved_partial_color_button#color in
	 (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
      (* 
       * mark_subtree_color = 
       * 	(let c = mark_subtree_color_button#color in
       * 	 (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
       *)
      existential_create_color = 
	(let c = ext_create_color_button#color in
	 (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));
      existential_instantiate_color = 
	(let c = ext_inst_color_button#color in
	 (Gdk.Color.red c, Gdk.Color.green c, Gdk.Color.blue c));

      display_doc_tooltips = doc_tooltip_check_box#active;
      display_turnstile_tooltips = turnstile_tooltip_check_box#active;
      display_command_tooltips = command_tooltip_check_box#active;

      default_width_proof_tree_window = 
	round_to_int default_size_width_adjustment#value;
      default_height_proof_tree_window = 
	round_to_int default_size_height_adjustment#value;

      internal_sequent_window_lines =
	round_to_int internal_seq_lines_adjustment#value;
      node_window_max_lines = 
	round_to_int external_node_lines_adjustment#value;

      debug_mode = debug_check_box#active;
      copy_input = tee_file_box_check_box#active;
      copy_input_file = tee_file_name_entry#text;
    }
    in
    update_sizes c

  (** Action for the Apply button: Extract a configuration record and
      update the current configuration. 
  *)
  method apply () =
    update_configuration (self#extract_configuration)

  (** Action for the Save button: Saves the current configuration in
      the user specific configuration file {!config_file_location}. If
      the values of this configuration dialog differ from the current
      configuration, a suitable warning is displayed. 
  *)
  method save () = 
    let do_save = ref true in
    if self#extract_configuration <> !current_config
    then begin
      let proceed_dialog = GWindow.message_dialog 
	~message:"The save operation writes the current configuration \
                  record to disk. However, the current configuration \
                  record differs from what the configuration dialog now \
                  shows (because there are changes that have not been \
                  applied). Proceed anyway?"
	~message_type:`QUESTION
	~buttons:GWindow.Buttons.yes_no ()
      in
      (match proceed_dialog#run () with
	| `YES -> ()
	| `NO 
	| `DELETE_EVENT -> do_save := false
      );
      proceed_dialog#destroy ()
    end;
    if !do_save 
    then
      try
	write_config_file config_file_location !current_config
      with
	| Sys_error s when Util.string_ends s "Permission denied" ->
	  run_message_dialog
	    ("No permission to write the configuration file at "
	     ^ config_file_location ^ "!")
	    `WARNING
	| e ->
	  let backtrace = Printexc.get_backtrace () in
	  let buf = Buffer.create 4095 in
	  let print_backtrace = ref !current_config.debug_mode in
	  (match e with 
	    | e ->
	      Buffer.add_string buf "Internal error: Escaping exception ";
	      Buffer.add_string buf (Printexc.to_string e);
	      Buffer.add_string buf " in write_config_file";
	      (match e with
		| U.Unix_error(error, _func, _info) ->
		  Buffer.add_char buf '\n';
		  Buffer.add_string buf (U.error_message error);
		| _ -> ()
	      )
	  );
	  if !print_backtrace then begin
	    Buffer.add_char buf '\n';
	    Buffer.add_string buf backtrace;
	  end;
	  prerr_endline (Buffer.contents buf);
	  run_message_dialog (Buffer.contents buf) `WARNING;
	  ()


  (** Action for the Restore button: Restore the configuration in the
      the user specific configuration file {!config_file_location} as
      current configuration and update this dialog accordingly.
  *)
  method restore () = 
    try
      let c = read_config_file config_file_location in
      self#set_configuration c;
      update_configuration c
    with
      | Sys_error s when Util.string_ends s "No such file or directory" ->
	run_message_dialog
	  ("No configuration file at " ^ config_file_location ^ "!")
	  `WARNING
      | Failure "Incompatible configuration file version" ->
	run_message_dialog
	  ("File " ^ config_file_location ^ 
	      " is not compatible with this version of Prooftree!")
	  `WARNING
      | Failure "Invalid configuration file" ->
	run_message_dialog
	  ("File " ^ config_file_location ^ " is not a valid Prooftree \
            configuration file!")
	  `WARNING
      | e ->
	let backtrace = Printexc.get_backtrace () in
	let buf = Buffer.create 4095 in
	let print_backtrace = ref !current_config.debug_mode in
	(match e with 
	  | e ->
	    Buffer.add_string buf "Internal error: Escaping exception ";
	    Buffer.add_string buf (Printexc.to_string e);
	    Buffer.add_string buf " in read_config_file";
	    (match e with
	      | U.Unix_error(error, _func, _info) ->
		Buffer.add_char buf '\n';
		Buffer.add_string buf (U.error_message error);
	      | _ -> ()
	    )
	);
	if !print_backtrace then begin
	  Buffer.add_char buf '\n';
	  Buffer.add_string buf backtrace;
	end;
	prerr_endline (Buffer.contents buf);
	run_message_dialog (Buffer.contents buf) `WARNING;
	()
	  

  (** Action for the Cancel button and the destroy signal. *)
  method destroy () =
    config_window := None;
    top_window#destroy()
      
  (** Action of the OK button. *)
  method ok () =
    self#apply ();
    self#destroy ()

end

(** [adjustment_set_pos_int ~lower adjustment] configures [adjustment]
    for integer values between [~lower] and [100].
*)
let adjustment_set_pos_int ?(lower = 1.0) (adjustment : GData.adjustment) =
  adjustment#set_bounds
    ~lower ~upper:100.0
    ~step_incr:1.0 ~page_incr:1.0 ()


(** Create a new configuation dialog. Creates the widget hierarchy,
    initializes the management object and registers all callbacks.
*)
let make_config_window () =
  let top_window = GWindow.window () in
  let top_v_box = GPack.vbox ~packing:top_window#add () in
  let _config_title = GMisc.label
    ~markup:"<big><b>Prooftree Configuration</b></big>"
    ~xpad:10 ~ypad:10
    ~packing:top_v_box#pack () in

  let notebook = GPack.notebook
    ~show_border:true
    ~packing:top_v_box#pack () in
  let append_to_notebook label =
    let label = GMisc.label ~markup:label () in
    fun w -> ignore(notebook#append_page ~tab_label:label#coerce w)
  in

  (****************************************************************************
   *
   * tree configuration frame 
   *
   ****************************************************************************)
  let tree_frame = GBin.frame 
    ~label:"Tree Layout Parameters"
    ~border_width:5
    ~packing:(append_to_notebook "Layout") () in
  let tree_frame_table = GPack.table 
    (* ~columns:2 ~rows:2 *) ~border_width:5
    ~packing:tree_frame#add () in
  let _middle_separator = GMisc.label ~text:"" ~xpad:7
    ~packing:(tree_frame_table#attach ~left:2 ~top:0) () in
  let _right_separator = GMisc.label ~text:"" ~xpad:2
    ~packing:(tree_frame_table#attach ~left:5 ~top:0) () in

  (* Line width *)
  let line_width_tooltip = "Line width of all lines" in
  let line_width_label = GMisc.label
    ~text:"Line width" ~xalign:0.0 ~xpad:5
    ~packing:(tree_frame_table#attach ~left:0 ~top:0) () in
  let line_width_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true 
    ~packing:(tree_frame_table#attach ~left:1 ~top:0) () in
  adjustment_set_pos_int line_width_spinner#adjustment;
  line_width_spinner#adjustment#set_value 
    (float_of_int !current_config.turnstile_line_width);
  line_width_label#misc#set_tooltip_text line_width_tooltip;
  line_width_spinner#misc#set_tooltip_text line_width_tooltip;

  (* turnstile radius *)
  let turnstile_size_tooltip = 
    "Radius of the circle around the current turnstile; determines \
     the size of the turnstile as well" in
  let turnstile_size_label = GMisc.label
    ~text:"Turnstile size" ~xalign:0.0 ~xpad:5
    ~packing:(tree_frame_table#attach ~left:0 ~top:1) () in
  let turnstile_size_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(tree_frame_table#attach ~left:1 ~top:1) () in
  adjustment_set_pos_int turnstile_size_spinner#adjustment;
  turnstile_size_spinner#adjustment#set_value
    (float_of_int !current_config.turnstile_radius);
  turnstile_size_label#misc#set_tooltip_text turnstile_size_tooltip;
  turnstile_size_spinner#misc#set_tooltip_text turnstile_size_tooltip;

  (* line_sep *)
  let line_sep_tooltip = 
    "Gap between the node connecting lines and the nodes" in
  let line_sep_label = GMisc.label
    ~text:"Line gap" ~xalign:0.0 ~xpad:5
    ~packing:(tree_frame_table#attach ~left:0 ~top:2) () in
  let line_sep_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(tree_frame_table#attach ~left:1 ~top:2) () in
  adjustment_set_pos_int ~lower:0.0 line_sep_spinner#adjustment;
  line_sep_spinner#adjustment#set_value
    (float_of_int !current_config.line_sep);
  line_sep_label#misc#set_tooltip_text line_sep_tooltip;
  line_sep_spinner#misc#set_tooltip_text line_sep_tooltip;

  (* subtree_sep *)
  let subtree_sep_tooltip =
    "Additional padding added to the width of each node in the proof tree" in
  let subtree_sep_label = GMisc.label
    ~text:"Node padding" ~xalign:0.0 ~xpad:5
    ~packing:(tree_frame_table#attach ~left:3 ~top:0) () in
  let subtree_sep_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(tree_frame_table#attach ~left:4 ~top:0) () in
  adjustment_set_pos_int ~lower:0.0 subtree_sep_spinner#adjustment;
  subtree_sep_spinner#adjustment#set_value
    (float_of_int !current_config.subtree_sep);
  subtree_sep_label#misc#set_tooltip_text subtree_sep_tooltip;
  subtree_sep_spinner#misc#set_tooltip_text subtree_sep_tooltip;

  (* proof_command_length *)
  let command_length_tooltip = 
    "Number of characters displayed for proof commands" in
  let command_length_label = GMisc.label
    ~text:"Command length" ~xalign:0.0 ~xpad:5
    ~packing:(tree_frame_table#attach ~left:3 ~top:1) () in
  let command_length_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(tree_frame_table#attach ~left:4 ~top:1) () in
  adjustment_set_pos_int command_length_spinner#adjustment;
  command_length_spinner#adjustment#set_value
    (float_of_int !current_config.proof_command_length);
  command_length_label#misc#set_tooltip_text command_length_tooltip;
  command_length_spinner#misc#set_tooltip_text command_length_tooltip;

  (* level distance *)
  let level_dist_tooltip = "Vertical distance between neighboring nodes" in
  let level_dist_label = GMisc.label
    ~text:"Vertical distance" ~xpad:5
    ~packing:(tree_frame_table#attach ~left:3 ~top:2) () in
  let level_dist_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(tree_frame_table#attach ~left:4 ~top:2) () in
  adjustment_set_pos_int level_dist_spinner#adjustment;
  level_dist_spinner#adjustment#set_value
    (float_of_int !current_config.level_distance);
  level_dist_label#misc#set_tooltip_text level_dist_tooltip;
  level_dist_spinner#misc#set_tooltip_text level_dist_tooltip;

  (****************************************************************************
   *
   * Fonts
   *
   ****************************************************************************)
  let font_frame = GBin.frame 
    ~label:"Fonts"
    ~border_width:5
    ~packing:(append_to_notebook "Fonts") () in
  let font_frame_table = GPack.table
    ~border_width:5 ~packing:font_frame#add () in

  (* tree font *)
  let tree_font_tooltip = "Font for proof commands in the proof tree display" in
  let tree_font_label = GMisc.label
    ~text:"Proof Tree" ~xalign:0.0 ~xpad:5
    ~packing:(font_frame_table#attach ~left:0 ~top:0) () in
  let tree_font_button = GButton.font_button
    ~title:"Proof Tree Font"
    ~font_name:!current_config.proof_tree_font
    ~packing:(font_frame_table#attach ~left:1 ~top:0) () in
  tree_font_button#set_use_size true;
  tree_font_button#set_use_font true;
  tree_font_label#misc#set_tooltip_text tree_font_tooltip;
  tree_font_button#misc#set_tooltip_text tree_font_tooltip;

  (* sequent font *)
  let sequent_font_tooltip = "Font for sequent and proof command windows" in
  let sequent_font_label = GMisc.label
    ~text:"Sequent window" ~xalign:0.0 ~xpad:5
    ~packing:(font_frame_table#attach ~left:0 ~top:1) () in
  let sequent_font_button = GButton.font_button
    ~title:"Sequent Window Font"
    ~font_name:!current_config.sequent_font
    ~packing:(font_frame_table#attach ~left:1 ~top:1) () in
  sequent_font_button#set_use_size true;
  sequent_font_button#set_use_font true;
  sequent_font_label#misc#set_tooltip_text sequent_font_tooltip;
  sequent_font_button#misc#set_tooltip_text sequent_font_tooltip;


  (****************************************************************************
   *
   * Colors
   *
   ****************************************************************************)
  let color_frame = GBin.frame 
    ~label:"Colors"
    ~border_width:5
    ~packing:(append_to_notebook "Colors") () in
  let color_frame_table = GPack.table
    ~border_width:5 ~packing:color_frame#add () in
  let _middle_separator = GMisc.label ~text:"" ~xpad:4
    ~packing:(color_frame_table#attach ~left:2 ~top:0) () in
  let _right_separator = GMisc.label ~text:"" ~xpad:2
    ~packing:(color_frame_table#attach ~left:5 ~top:0) () in

  let make_color_conf row column color 
      label_text selection_dialog_title tooltip =
    let label = GMisc.label
      ~text:label_text ~xalign:0.0 ~xpad:5
      ~packing:(color_frame_table#attach ~left:column ~top:row) () in
    let button = GButton.color_button
      ~title:selection_dialog_title
      ~color:color
      ~packing:(color_frame_table#attach ~left:(column + 1) ~top:row) () in
    (* button#set_use_alpha true; *)
    label#misc#set_tooltip_text tooltip;
    button#misc#set_tooltip_text tooltip;
    (label, button)
  in

  let row = 0 in
  let column = 0 in

  (* current color *)
  let (current_color_label, current_color_button) =
    make_color_conf row column !current_gdk_color "Current branch" 
      "Current Branch Color"
      "Color for the current branch" in

  let column = column + 3 in

  (* proved incomplete color *)
  let (proved_incomplete_color_label, proved_incomplete_color_button) =
    make_color_conf row column !proved_incomplete_gdk_color "Proved incomplete"
      "Incompletely Proved Branches Color"
      "Color for proved branches which still have some non-instantiated \
       existential variables" in

  let row = 1 in
  let column = 0 in

  (* cheated color *)
  let (cheated_color_label, cheated_color_button) =
    make_color_conf row column !cheated_gdk_color "Cheated"
      "Cheated Branches Color"
      "Color for branches terminated with a cheating proof command" in

  let column = column + 3 in

  (* existential create color *)
  let (ext_create_color_label, ext_create_color_button) =
    make_color_conf row column !existential_create_gdk_color
      "Create existential"
      "Create Existential Variable Color"
      "Color for marking the node that introduces some existential variable" in

  let row = 2 in
  let column = 0 in

  (* 
   * (\* mark subtree color *\)
   * let (mark_subtree_color_label, mark_subtree_color_button) =
   *   make_color_conf row column !mark_subtree_gdk_color "Mark"
   *     "Mark Subtree Color"
   *     "Color for marking subtrees, e.g., those that contain a certain \
   *      existential variable" in
   *)

  (* proved complete color *)
  let (proved_complete_color_label, proved_complete_color_button) =
    make_color_conf row column !proved_complete_gdk_color "Proved complete"
      "Completely Proved Branches Color"
      "Color for completely proved branches where all existential \
       variables are fully instantiated" in

  let column = column + 3 in

  (* existential instantiate color *)
  let (ext_inst_color_label, ext_inst_color_button) =
    make_color_conf row column !existential_instantiate_gdk_color
      "Instantiate existential"
      "Instantiate Existential Variable Color"
      "Color for marking the node that instantiates some existential variable"
  in

  let row = 3 in
  let column = 0 in

  (* proved partial color *)
  let (proved_partial_color_label, proved_partial_color_button) =
    make_color_conf row column !proved_partial_gdk_color "Proved partial"
      "Partially Proved Branches Color"
      "Color for completely proved branches where all existential \
       variables are instantiated but some of them use not-yet \
       instantiated existential variables" in



  (****************************************************************************
   *
   * Misc
   *
   ****************************************************************************)
  let misc_frame = GBin.frame 
    ~label:"Miscellaneous"
    ~border_width:5
    ~packing:(append_to_notebook "Misc") () in
  let misc_frame_table = GPack.table 
    (* ~columns:2 ~rows:2 *) ~border_width:5
    ~packing:misc_frame#add () in

  (* doc tooltips *)
  let doc_tooltip_tooltip = "Switch ordinary help tool tips on and off" in
  let doc_tooltip_alignment = GBin.alignment
    ~padding:(0,0,3,0)
    ~packing:(misc_frame_table#attach ~left:0 ~right:2 ~top:0) () in
  let doc_tooltip_check_box = GButton.check_button
    ~label:"Display help tool tips"
    ~active:!current_config.display_doc_tooltips
    ~packing:doc_tooltip_alignment#add () in
  doc_tooltip_alignment#misc#set_tooltip_text doc_tooltip_tooltip;

  (* turnstile tooltips *)
  let turnstile_tooltip_tooltip = 
    "Switch sequent display as tool tip over the proof tree on and off" in
  let turnstile_tooltip_alignment = GBin.alignment
    ~padding:(0,0,3,0)
    ~packing:(misc_frame_table#attach ~left:0 ~right:2 ~top:1) () in
  let turnstile_tooltip_check_box = GButton.check_button
    ~label:"Display turnstile tool tips"
    ~active:!current_config.display_turnstile_tooltips
    ~packing:turnstile_tooltip_alignment#add () in
  turnstile_tooltip_alignment#misc#set_tooltip_text turnstile_tooltip_tooltip;

  (* command tooltips *)
  let command_tooltip_tooltip = 
    "Switch display of truncated commands as tool tip on and off" in
  let command_tooltip_alignment = GBin.alignment
    ~padding:(0,0,3,0)
    ~packing:(misc_frame_table#attach ~left:0 ~right:2 ~top:2) () in
  let command_tooltip_check_box = GButton.check_button
    ~label:"Display command tool tips"
    ~active:!current_config.display_command_tooltips
    ~packing:command_tooltip_alignment#add () in
  command_tooltip_alignment#misc#set_tooltip_text command_tooltip_tooltip;

  (* drag accel *)
  let drag_accel_tooltip = 
    "Acceleration for dragging the viewport to the proof tree" in
  let drag_accel_label = GMisc.label
    ~text:"Drag acceleration" ~xalign:0.0 ~xpad:5
    ~packing:(misc_frame_table#attach ~left:0 ~top:3) () in
  let drag_accel_spinner = GEdit.spin_button
    ~digits:2 ~numeric:true
    ~packing:(misc_frame_table#attach ~left:1 ~top:3) () in
  drag_accel_spinner#adjustment#set_bounds
    ~lower:(-99.0) ~upper:99.0
    ~step_incr:0.01 ~page_incr:1.0 ();
  drag_accel_spinner#adjustment#set_value
    !current_config.button_1_drag_acceleration;
  drag_accel_label#misc#set_tooltip_text drag_accel_tooltip;
  drag_accel_spinner#misc#set_tooltip_text drag_accel_tooltip;

  (* default size *)
  let default_size_tooltip = "Size for newly created proof tree windows" in
  let default_size_label = GMisc.label
    ~text:"Default window size" ~xalign:0.0 ~xpad:5
    ~packing:(misc_frame_table#attach ~left:0 ~top:4) () in
  let default_size_width_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(misc_frame_table#attach ~left:1 ~top:4) () in
  default_size_width_spinner#adjustment#set_bounds
    ~lower:(-9999.0) ~upper:9999.0
    ~step_incr:1.0 ~page_incr:100.0 ();
  default_size_width_spinner#adjustment#set_value
    (float_of_int !current_config.default_width_proof_tree_window);
  let _x_label = GMisc.label
    ~text:"\195\151" (* multiplication sign U+00D7 *)
    ~xpad:5
    ~packing:(misc_frame_table#attach ~left:2 ~top:4) () in
  let default_size_height_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(misc_frame_table#attach ~left:3 ~top:4) () in
  default_size_height_spinner#adjustment#set_bounds
    ~lower:(-9999.0) ~upper:9999.0
    ~step_incr:1.0 ~page_incr:100.0 ();
  default_size_height_spinner#adjustment#set_value
    (float_of_int !current_config.default_height_proof_tree_window);
  default_size_label#misc#set_tooltip_text default_size_tooltip;
  default_size_width_spinner#misc#set_tooltip_text default_size_tooltip;
  default_size_height_spinner#misc#set_tooltip_text default_size_tooltip;

  (* internal sequent window lines *)
  let internal_seq_lines_tooltip = 
    "Initial height (in lines) of the sequent window \
     below the proof tree display" 
  in
  let internal_seq_lines_label = GMisc.label
    ~text:"Int. Sequent window" ~xalign:0.0 ~xpad:5
    ~packing:(misc_frame_table#attach ~left:0 ~top:5) () in
  let internal_seq_lines_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(misc_frame_table#attach ~left:1 ~top:5) () in
  adjustment_set_pos_int ~lower:0.0 internal_seq_lines_spinner#adjustment;
  internal_seq_lines_spinner#adjustment#set_value
    (float_of_int !current_config.internal_sequent_window_lines);
  internal_seq_lines_label#misc#set_tooltip_text internal_seq_lines_tooltip;
  internal_seq_lines_spinner#misc#set_tooltip_text internal_seq_lines_tooltip;

  (* external node window lines *)
  let external_node_lines_tooltip = 
    "Maximal height (in lines) of additional node windows" in
  let external_node_lines_label = GMisc.label
    ~text:"Ext. node window" ~xalign:0.0 ~xpad:5
    ~packing:(misc_frame_table#attach ~left:0 ~top:6) () in
  let external_node_lines_spinner = GEdit.spin_button
    ~digits:0 ~numeric:true
    ~packing:(misc_frame_table#attach ~left:1 ~top:6) () in
  adjustment_set_pos_int external_node_lines_spinner#adjustment;
  external_node_lines_spinner#adjustment#set_value
    (float_of_int !current_config.node_window_max_lines);
  external_node_lines_label#misc#set_tooltip_text external_node_lines_tooltip;
  external_node_lines_spinner#misc#set_tooltip_text external_node_lines_tooltip;

  (* non-configurable config-file *)
  let config_file_tooltip = 
    "The configuration file is determined at compilation time" in
  let config_file_label = GMisc.label
    ~text:"Configuration file"
    ~xalign:0.0 ~xpad:5
    ~packing:(misc_frame_table#attach ~left:0 ~top:7) () in
  let config_file_alignment = GBin.alignment
    ~padding:(0,0,3,0)
    ~packing:(misc_frame_table#attach ~left:1 ~right:4 ~top:7) () in
  let _config_file_file = GMisc.label
    ~text:config_file_location
    ~xalign:0.0
    ~packing:config_file_alignment#add () in
  config_file_label#misc#set_tooltip_text config_file_tooltip;
  config_file_alignment#misc#set_tooltip_text config_file_tooltip;

  (****************************************************************************
   *
   * Debugging Options
   *
   ****************************************************************************)
  let debug_frame = GBin.frame 
    ~label:"Debugging Options"
    ~border_width:5
    ~packing:(append_to_notebook "Debug") () in
  let debug_frame_table = GPack.table 
    (* ~columns:2 ~rows:2 *) ~border_width:5
    ~packing:debug_frame#add () in

  (* debug *)
  let debug_tooltip = "Provide more information on fatal error conditions" in
  let debug_alignment = GBin.alignment
    ~padding:(0,0,3,0)
    ~packing:(debug_frame_table#attach ~left:0 ~right:4 ~top:0) () in
  let debug_check_box = GButton.check_button
    ~label:"More debug information"
    ~active:!current_config.debug_mode
    ~packing:debug_alignment#add () in
  debug_alignment#misc#set_tooltip_text debug_tooltip;

  (* tee file checkbox*)
  let tee_file_box_tooltip = "Save all input from Proof General in log file" in
  let tee_file_box_alignment = GBin.alignment
    ~padding:(0,0,3,0)
    ~packing:(debug_frame_table#attach ~left:0 ~right:4 ~top:1) () in
  let tee_file_box_check_box = GButton.check_button
    ~label:"Log Proof General input"
    ~active:!current_config.copy_input
    ~packing:tee_file_box_alignment#add () in
  tee_file_box_alignment#misc#set_tooltip_text tee_file_box_tooltip;

  (* tee file filename *)
  let tee_file_name_label = GMisc.label
    ~text:"Log file" ~xalign:0.0 ~xpad:5
    ~packing:(debug_frame_table#attach ~left:0 ~top:2) () in
  let tee_file_name_entry = GEdit.entry
    ~text:!current_config.copy_input_file
    (* ~max_length:25 *)
    ~packing:(debug_frame_table#attach ~left:1 ~top:2) () in
  let _button_separator = GMisc.label ~text:"" ~xpad:5
    ~packing:(debug_frame_table#attach ~left:2 ~top:2) () in
  let tee_file_name_button = GButton.button
    ~label:"Log-file selection dialog"
    ~packing:(debug_frame_table#attach ~left:3 ~top:2) () in


  (****************************************************************************
   *
   * bottom button box
   *
   ****************************************************************************)
  (* 
   * let _separator = GMisc.separator `HORIZONTAL 
   *   ~packing:top_v_box#pack () in
   *)
  let button_box = GPack.hbox 
    ~spacing:5 (* ~border_width:5 *) ~packing:top_v_box#pack () in
  let reset_button = GButton.button 
    ~label:"Set defaults" ~packing:button_box#pack () in
  let apply_button = GButton.button
    ~label:"Apply" ~packing:button_box#pack () in
  let cancel_button = GButton.button
    ~label:"Cancel" ~packing:button_box#pack () in
  let ok_button = GButton.button
    ~label:"OK" ~packing:button_box#pack () in
  let restore_button = GButton.button
    ~label:"Restore" ~packing:(button_box#pack ~from:`END) () in
  let save_button = GButton.button
    ~label:"Save" ~packing:(button_box#pack ~from:`END) () in
  let config_window = 
    new config_window top_window 
      line_width_spinner
      turnstile_size_spinner
      line_sep_spinner
      subtree_sep_spinner
      command_length_spinner
      level_dist_spinner
      tree_font_button
      sequent_font_button
      current_color_button
      cheated_color_button
      proved_complete_color_button
      proved_incomplete_color_button
      proved_partial_color_button
      (* mark_subtree_color_button *)
      ext_create_color_button
      ext_inst_color_button
      drag_accel_spinner
      doc_tooltip_check_box
      turnstile_tooltip_check_box
      command_tooltip_check_box
      default_size_width_spinner default_size_height_spinner
      internal_seq_lines_spinner
      external_node_lines_spinner
      debug_check_box
      tee_file_box_check_box 
      tee_file_name_label tee_file_name_entry tee_file_name_button
      [ line_width_label#misc; line_width_spinner#misc;
	turnstile_size_label#misc; turnstile_size_spinner#misc;
	line_sep_label#misc; line_sep_spinner#misc;
	subtree_sep_label#misc; subtree_sep_spinner#misc;
	command_length_label#misc; command_length_spinner#misc;
	level_dist_label#misc; level_dist_spinner#misc;
	tree_font_label#misc; tree_font_button#misc;
	sequent_font_label#misc; sequent_font_button#misc;
	current_color_label#misc; current_color_button#misc;
	cheated_color_label#misc; cheated_color_button#misc;
	proved_complete_color_label#misc; proved_complete_color_button#misc;
	proved_incomplete_color_label#misc; proved_incomplete_color_button#misc;
	proved_partial_color_label#misc; proved_partial_color_button#misc;
	(* mark_subtree_color_label#misc; mark_subtree_color_button#misc; *)
	ext_create_color_label#misc; ext_create_color_button#misc;
	ext_inst_color_label#misc; ext_inst_color_button#misc;
	doc_tooltip_alignment#misc;
	turnstile_tooltip_alignment#misc;
	command_tooltip_alignment#misc;
	drag_accel_label#misc; drag_accel_spinner#misc;
	default_size_label#misc; default_size_width_spinner#misc;
	default_size_height_spinner#misc; 
	internal_seq_lines_label#misc; internal_seq_lines_spinner#misc;
	external_node_lines_label#misc; external_node_lines_spinner#misc;
	config_file_label#misc;
	config_file_alignment#misc; debug_alignment#misc;
	tee_file_box_alignment#misc;
      ]
  in

  top_window#set_title "Prooftree Configuration";
  config_window#toggle_tooltips ();
  config_window#tee_file_toggle();
  ignore(doc_tooltip_check_box#connect#toggled
	   ~callback:config_window#toggle_tooltips);
  ignore(tee_file_box_check_box#connect#toggled 
	   ~callback:config_window#tee_file_toggle);
  ignore(tee_file_name_button#connect#clicked 
	   ~callback:config_window#tee_file_button_click);
  ignore(top_window#connect#destroy ~callback:config_window#destroy);
  ignore(reset_button#connect#clicked ~callback:config_window#reset_to_default);
  ignore(apply_button#connect#clicked ~callback:config_window#apply);
  ignore(cancel_button#connect#clicked ~callback:config_window#destroy);
  ignore(ok_button#connect#clicked ~callback:config_window#ok);
  ignore(save_button#connect#clicked ~callback:config_window#save);
  ignore(restore_button#connect#clicked ~callback:config_window#restore);
  top_window#show ();

  config_window


(** Show a configuration dialog. If there is currently none, a new one
    is created.
*)
let show_config_window () =
  match !config_window with
    | Some w -> w#present
    | None -> config_window := Some(make_config_window ())
