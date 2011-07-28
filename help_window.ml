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
 * $Id: help_window.ml,v 1.2 2011/07/28 12:53:07 tews Exp $
 *)


(** Creation and display of the help window *)

type tags_symbols =
  | Default
  | Blue
  | Red
  | Brown
  | Black
  | Italic
  | Bold

let help_text = 
  [(Default, "The meaning of the colors in the proof tree is as follows: ");
   (Blue, "blue branches");
   (Default, " have been proven, the branch to the current goal \
              (which is marked with a circle) is ");
   (Brown, "colored brown");
   (Default, ", ");
   (Black, "black branches");
   (Default, " end in a currently open goal, and ");
   (Red, "red branches");
   (Default, " have been proved with a cheating command such as ");
   (Italic, "admit");
   (Default, ". Colors as well as many proof-tree layout parameters \
can be changed in the Configuration dialog.

In addition to scroll bars and cursor keys one can move the proof \
tree by dragging with mouse button 1 pressed. By default, dragging \
moves the viewport (i.e., the proof tree underneath moves in the \
opposite direction). After setting a negative value for ");
   (Italic, "Drag acceleration");
   (Default, " in the Configuration dialog, dragging will move \
the proof tree instead.

The sequent window normally shows the ancestor sequent of the current \
goal. With a single left mouse click one can display any goal or proof command \
in the sequent window. A single click outside the proof tree will switch \
back to default behavior.

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
   (Italic, "Exit");
   (Default, " item terminates ");
   (Bold, "Prooftree");
   (Default, " and closes all proof windows.")
  ]

let fill_help_buffer (buffer :GText.buffer) =
  let blue_tag = buffer#create_tag [`FOREGROUND "blue"] in
  let red_tag = buffer#create_tag [`FOREGROUND "red"] in
  let brown_tag = buffer#create_tag [`FOREGROUND "brown"] in
  let black_tag = 
    buffer#create_tag [(`FOREGROUND "white"); (`BACKGROUND "black")] 
  in
  let i_tag = buffer#create_tag [`FONT "italic"] in
  let bold_tag = buffer#create_tag [`FONT "bold"] in
  let get_tags = function
    | Default -> []
    | Blue -> [blue_tag]
    | Red -> [red_tag]
    | Brown -> [brown_tag]
    | Black -> [black_tag]
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

