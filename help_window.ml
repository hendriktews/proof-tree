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


(** Creation and display of the help window *)

(** {2 General remarks}

    The help text is formatted inside a GText.buffer, which is
    displayed in a GText.view. The help text needs some basic markup
    (colors, italic, bold). The markup system for Gdk text buffers is
    rather heavy weight and there is apparently no simple way to use
    some basic markup.

    The help text uses therefore its own rudimentary markup system.
    The help text itself is a list of pairs, where each pair consists
    of a style-tag and a string. The string is the help text and the
    style-tag (of type {!tags_symbols} determines the markup. To
    create the help text, one iterates over the list, translates the
    style-tags into appropriate {xref lablgtk class GText.tag}'s and
    inserts the text.
*)


open Configuration


(** {2 Module contents} *)


(** Style-tags for the help text *)
type tags_symbols =
  | Default				(** Default, no style *)
  | Color of Gdk.color			(** Foreground color *)
  | Background of Gdk.color		(** Background color *)
  | Italic				(** Set in italic *)
  | Bold				(** Set in bold *)
  | Heading                             (** bold and large *)


(** The help text *)
let help_text = 
  let bold_proof_tree = (Bold, "Prooftree") in
  let bold_proof_general = (Bold, "Proof General")
  in
  (**************************************************************************)
  (***************************** Colors *************************************)
  (**************************************************************************)
  [(Heading, "Colors");
   (Default, "\n\nThe meaning of the colors in the proof tree is as follows: ");
   (Color !proved_complete_gdk_color, 
    "completely proved branches (green by default), ");
   (Color !proved_incomplete_gdk_color,
    "proved branches with some not (yet) instantiated existential variables \
(cyan by default), ");
   (Color !proved_partial_gdk_color,
    "proved branches with all their own existential variables instantiated \
that nevertheless depend on some not (yet) instantiated existential variable \
(dark green by default), ");
   (Color !current_gdk_color, "branch to the current goal (blue by default), ");
   (Default, "currently open branches (default foreground color) and ");
   (Color !cheated_gdk_color, 
    "branches terminated with a cheating command such as admit");
   (Default, ". Colors as well as many ");
   bold_proof_tree;
   (Default, " parameters \
can be changed in the configuration dialog.\n\
\n");
   (*************************************************************************)
   (***************************** Navigation ********************************)
   (*************************************************************************)
   (Heading, "Navigation");
   (Default, "\n\n\
In addition to scroll bars and the usual keys one can move the proof \
tree by dragging with mouse button 1 pressed. By default, dragging \
moves the viewport (i.e., the proof tree underneath moves in the \
opposite direction). After setting a negative value for ");
   (Italic, "Drag acceleration");
   (Default, " in the Configuration dialog, dragging will move \
the proof tree instead (i.e, the proof tree moves in the same \
direction as the mouse).\n\
\n");
   (*************************************************************************)
   (***************************** Sequent window ****************************)
   (*************************************************************************)
   (Heading, "The sequent window and additional displays");
   (Default, "\n\n\
The sequent display below the proof tree \
normally shows the ancestor sequent of the current \
goal. With a single left mouse click one can display any goal or proof command \
in the sequent display. A single click outside the proof tree will switch \
back to default behavior. The initial size of the sequent display can \
be set in the configuration dialog. A value of 0 hides the sequent display.\n\
\n\
A double click or a shift-click displays any goal or proof command \
in an additional window. These additional windows are automatically updated, \
for instance, if an existential variable is instantiated. For additional \
sequent displays one can browse the instantiation history of the sequent \
using the forward and backward buttons. These additional windows can be ");
   (Italic, "detached");
   (Default, " from the proof tree. A detached \
display is neither automatically updated nor automatically deleted.\n\
\n\
");
   (*************************************************************************)
   (***************************** Tooltips **********************************)
   (*************************************************************************)
   (Heading, "Tooltips");
   (Default, "\n\n\
If turnstile tool tips are switched on, the complete sequent text is \
displayed as toop tip when the mouse stays above a sequent symbol in the \
proof tree display. Similar for command tool tips and proof commands.\n\
\n\
Long proof commands are truncated with \226\128\166 in the display. The \
length at which truncation happens can be set in the configuration dialog. \
Any truncated proof command is displayed in full length as tool tip if the \
mouse stays long enough above it (and if command tool tips are enabled).\n\
\n");
   (*************************************************************************)
   (***************************** Existentials ******************************)
   (*************************************************************************)
   (Heading, "Existential variables");
   (Default, "\n\
\n");
   bold_proof_tree;
   (Default, " keeps track of existential variables, whether they \
have been instantiated and whether they depend on some other, \
not (yet) instantiated existential. \
It uses different colors for proved branches that contain non-instantiated \
existential variables and branches that only depend on some not instantiated \
existential. Displays with sequents \
or proof commands (in tool-tips and in additional windows) list those \
existential variables that are currently not (yet) instantiated.\n\
\n\
The menu item ");
   (Italic, "Existentials");
   (Default, " opens the dialog for existential variables, which contains \
a table with all existential variables of the current proof and their \
dependencies. \
For each existential variable, the table contains a ");
   (Italic, "Mark");
   (Default, " button, which marks the proof command that introduced \
this variable ");
   (Background !existential_create_gdk_color,
    "(with yellow background, by default)");
   (Default, " and, if present, the proof command that instantiated \
this variable ");
   (Background !existential_instantiate_gdk_color,
    "(with orange background, by default)");
   (Default, " in the proof-tree display.\n\
\n\
With ");
   (Bold, "Coq");
   (Default, " >= 8.5, existential variables are severely broken in ");
   bold_proof_tree;
   (Default, " because ");
   (Bold, "Coq");
   (Default, " does not provide the necessary information, see ");
   (Bold, "Coq");
   (Default, " bug 4504.\n\
\n");
   (*************************************************************************)
   (***************************** Menus *************************************)
   (*************************************************************************)
   (Heading, "Menus");
   (Default, "\n\n\
The menu button opens the main menu. A right click opens the context menu, \
which contains some additional items. \n\
\n\
The item ");
   (Italic, "Undo to point");
   (Default, ", which is only active over a sequent node in the proof \
tree display, sends an appropriate retract command to ");
   bold_proof_general;
   (Default, ".\n\
\n\
The items ");
   (Italic, "Insert command");
   (Default, " and ");
   (Italic, "Insert subtree");
   (Default, " cause ");
   bold_proof_general;
   (Default, " to insert, respectively, the selected proof command or all \
proof commands of the selected subtree, at point in the selected buffer.\n\
\n\
The ");
   (Italic, "Clone");
   (Default, " menu item clones the current proof tree in a separate \
proof tree window. This cloned proof tree is not connected with ");
   bold_proof_general;
   (Default, " and won't be updated when the proof is changed.\n\
\n\
The ");
   (Italic, "Show current");
   (Default, " and ");
   (Italic, "Show selected");
   (Default, " menu items reposition the proof tree such that the \
current proof goal or, respectively, the selected node is visible.\n\
\n\
The item ");
   (Italic, "Existentials");
   (Default, " opens the dialog for existential variables, see above.\n\
\n\
The ");
   (Italic, "Configuration");
   (Default, " item displays the configuration dialog. Changes in this dialog \
immediately take effect. In this dialog, the ");
   (Italic, "Save");
   (Default, " button stores the current configuration values \
              in the file ");
   (Italic, config_file_location);
   (Default, ", which overwrites the build-in default configuration \
at start up. The ");
   (Italic, "Cancel");
   (Default, " button closes the dialog and resets the configuration to \
the state before starting the configuration dialog.\n\
\n\
The ");
   (Italic, "Exit");
   (Default, " item terminates ");
   bold_proof_tree;
   (Default, " and closes all proof windows. (Closing all windows does ");
   (Italic, "not");
   (Default, " terminate ");
   bold_proof_tree;
   (Default, ".)\n\
\n");
   (*************************************************************************)
   (***************************** Customization *****************************)
   (*************************************************************************)
   (Heading, "Customization");
   (Default, "\n\n\
A major part of the proof visualization task is done by ");
   bold_proof_general;
   (Default, ". Therefore, certain aspects can only be configured \
inside ");
   bold_proof_general;
   (Default, " in the customization groups ");
   (Italic, "proof-tree");
   (Default, " and ");
   (Italic, "proof-tree-internals. ");
   (Default, "For instance, ");
   bold_proof_tree;
   (Default, " command line arguments or the regular expressions for \
navigation and cheating commands can be configured there. \
To visit a customization group, type ");
   (Italic, "M-x customize-group");
   (Default, " followed by the name of the customization group inside ");
   bold_proof_general;
  ]


(** Format {!help_text} and insert it into the given buffer. The
    style-tags are translated into appropriate {xref lablgtk type
    GText.tag_property}, which are used to create {xref lablgtk class
    GText.tag}'s, which in turn are used to insert the help text.
*)
let fill_help_buffer (buffer : GText.buffer) =
  let heading_tag = buffer#create_tag [`SCALE `LARGE; `FONT "bold"] in
  let i_tag = buffer#create_tag [`FONT "italic"] in
  let bold_tag = buffer#create_tag [`FONT "bold"] in
  let get_tags = function
    | Default -> []
    | Color gdk_color -> [buffer#create_tag [`FOREGROUND_GDK gdk_color]]
    | Background gdk_color -> [buffer#create_tag [`BACKGROUND_GDK gdk_color]]
    | Italic -> [i_tag]
    | Bold -> [bold_tag]
    | Heading -> [heading_tag]
  in
  List.iter
    (fun (tag_sym, text) -> buffer#insert ~tags:(get_tags tag_sym) text)
    help_text


(** Flag for option [-help]. *)
let start_help_dialog = ref false


(** Create and display a new help window. This function creates a new
    {xref lablgtk class GWindow.dialog} that contains the formatted
    help text inside a {xref lablgtk class GText.view}.
*)
let show_help_window () =
  let help_win = 
    GWindow.dialog
      ~no_separator:true
      ~title:"Prooftree Help"
      ~resizable:true
      ()
  in
  let close_fun _ =
    help_win#destroy ();
    if !start_help_dialog then exit 0
  in
  help_win#add_button_stock `CLOSE `CLOSE;
  let _help_title =
    GMisc.label
      ~markup:"<big><b>Prooftree Help</b></big>"
      ~selectable:true ~xpad:10 ~ypad:10
      ~packing:help_win#vbox#pack ()
  in
  let help_scrolling = 
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:(help_win#vbox#pack ~expand:true) () 
  in
  let help_view =
    GText.view 
      ~border_width:2
      ~editable:false 
      ~cursor_visible:false
      ~wrap_mode:`WORD
      ~packing:help_scrolling#add ()
  in
  fill_help_buffer help_view#buffer;
  ignore(help_win#connect#destroy ~callback:close_fun);
  ignore(help_win#connect#response ~callback:close_fun);
  help_win#set_default_size ~width:400 ~height:300;
  (* help_win#set_default_size ~width:800 ~height:800; *)
  help_win#show ()

