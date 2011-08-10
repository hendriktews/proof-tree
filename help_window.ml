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
 * $Id: help_window.ml,v 1.3 2011/08/10 12:01:18 tews Exp $
 *)


(** Creation and display of the help window *)

open Configuration

type tags_symbols =
  | Default
  | Proved_color
  | Cheated_color
  | Current_color
  | Italic
  | Bold

let help_text = 
  [(Default, "The meaning of the colors in the proof tree is as follows: ");
   (Proved_color, "proved branches (green by default), ");
   (Current_color, "branch to the current goal (blue by default), ");
   (Default, "currently open branches (default foreground color) and ");
   (Cheated_color, "branches terminated with a cheating command such as admit");
   (Default, ". Colors as well as many ");
   (Bold, "Prooftree");
   (Default, " parameters \
can be changed in the configuration dialog.

In addition to scroll bars and cursor keys one can move the proof \
tree by dragging with mouse button 1 pressed. By default, dragging \
moves the viewport (i.e., the proof tree underneath moves in the \
opposite direction). After setting a negative value for ");
   (Italic, "Drag acceleration");
   (Default, " in the Configuration dialog, dragging will move \
the proof tree instead (i.e, the proof tree moves in the same \
direction as the mouse).

The sequent display normally shows the ancestor sequent of the current \
goal. With a single left mouse click one can display any goal or proof command \
in the sequent display. A single click outside the proof tree will switch \
back to default behavior. The initial size of the sequent display can \
be set in the configuration dialog. A value of 0 hides the sequent display.

Long proof commands are truncated with \226\128\166 in the display. The \
length at which truncation happens can be set in the configuration dialog. \
Any truncated proof command is displayed in full length as tool tip if the \
mouse stays long enough above it (and if tool tips are enabled).

A double click displays any goal or proof command in an additional \
window. These additional windows are deleted when the main proof-tree \
window disappears, unless the ");
   (Italic, "Sticky");
   (Default, " button is pressed.

A right click or a click on the menu button opens the main menu. The ");

   (Italic, "Clone");
   (Default, " menu item clones the current proof tree in a separate \
proof tree window. This cloned proof tree is not connected with Proof \
General and won't be updated when the proof is changed.

The ");
   (Italic, "Show current");
   (Default, " menu item repositions the proof tree such that the \
current proof goal is visible.

The ");
   (Italic, "Configuration");
   (Default, " item displays the configuration dialog. Changing values \
there does only take effect after the ");
   (Italic, "Apply");
   (Default, " or ");
   (Italic, "OK");
   (Default, " button has been pressed. The ");
   (Italic, "Save");
   (Default, " button stores the current configuration values \
              in the file ");
   (Italic, config_file_location);
   (Default, ", which overwrites the build-in default configuration \
at start up.

The ");
   (Italic, "Exit");
   (Default, " item terminates ");
   (Bold, "Prooftree");
   (Default, " and closes all proof windows.

A major part of the proof visualization task is done by ");
   (Bold, "Proof General");
   (Default, ". Therefore, certain aspects can only be configured \
inside ");
   (Bold, "Proof General");
   (Default, " in the customization groups ");
   (Italic, "proof-tree");
   (Default, " and ");
   (Italic, "proof-tree-internals. ");
   (Default, "For instance, ");
   (Bold, "Prooftree");
   (Default, " command line arguments or the regular expressions for \
navigation and cheating commands can be configured there. \
To visit a customization group, type ");
   (Italic, "M-x customize-group");
   (Default, " followed by the name of the customization group inside ");
   (Bold, "Proof General.");
  ]

let fill_help_buffer (buffer :GText.buffer) =
  let proved_tag = buffer#create_tag [`FOREGROUND_GDK !proved_gdk_color] in
  let cheated_tag = buffer#create_tag [`FOREGROUND_GDK !cheated_gdk_color] in
  let current_tag = buffer#create_tag [`FOREGROUND_GDK !current_gdk_color] in
  let i_tag = buffer#create_tag [`FONT "italic"] in
  let bold_tag = buffer#create_tag [`FONT "bold"] in
  let get_tags = function
    | Default -> []
    | Proved_color -> [proved_tag]
    | Cheated_color -> [cheated_tag]
    | Current_color -> [current_tag]
    | Italic -> [i_tag]
    | Bold -> [bold_tag]
  in
  List.iter
    (fun (tag_sym, text) -> buffer#insert ~tags:(get_tags tag_sym) text)
    help_text


let show_help_window () =
  let help_win = 
    GWindow.dialog
      ~no_separator:true
      ~title:"Prooftree Help"
      ~resizable:true
      ()
  in
  help_win#add_button "Close" `CLOSE;
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
  ignore(help_win#connect#destroy ~callback:(fun () -> help_win#destroy()));
  ignore(help_win#connect#response ~callback:(fun _ -> help_win#destroy()));
  help_win#set_default_size ~width:400 ~height:300;
  help_win#show ()

