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
 * $Id: node_window.ml,v 1.2 2011/05/28 19:22:00 tews Exp $
 *)


(** Creation, display and drawing of the extra node windows *)


open Configuration
open Gtk_ext
open Draw_tree

class type proof_window_interface =
object
  method invalidate_drawing_area : unit
end


class node_window proof_window node top_window text_window 
  sticky_button window_number =
object (self)

  (***************************************************************************
   *
   * Internal state and setters/accessors
   *
   ***************************************************************************)
  val proof_window = proof_window
  val node = node
  val top_window = (top_window : GWindow.window)
  val text_window = text_window
  val sticky_button = sticky_button
  val window_number = window_number

  method window_number = window_number

  method update_content new_content =
    text_window#buffer#set_text new_content

  method delete_node_window () =
    node#delete_external_window (self :> external_node_window);
    proof_window#invalidate_drawing_area;
    top_window#destroy()

  method private delete_node_window_event _ =
    self#delete_node_window ();
    true

  method delete_node_window_maybe =
    if not sticky_button#active then self#delete_node_window ()

  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
      | ks when (ks = GdkKeysyms._Q or ks = GdkKeysyms._q)  -> 
	self#delete_node_window_event ev

      | _ -> false

  initializer 
    node#register_external_window (self :> external_node_window)

end


let make_node_window proof_window proof_name node window_number =
  let top_window = GWindow.window () in
  let top_v_box = GPack.vbox ~packing:top_window#add () in
  let scrolling = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:top_v_box#pack ()
  in
  let text_win = 
    GText.view ~editable:false ~cursor_visible:false
      ~packing:scrolling#add ()
  in
  let context = text_win#misc#pango_context in
  let metrics = context#get_metrics() in
  let char_width = GPango.to_pixels(metrics#approx_char_width) in
  let layout = context#create_layout in
  Pango.Layout.set_text layout "X";
  let (_, char_height) = Pango.Layout.get_pixel_size layout in
  let lines = 
    min (Util.number_of_lines node#content) 
      !current_config.node_window_max_lines
  in
  text_win#misc#set_size_request 
    ~width:(char_width * 80) ~height:(char_height * lines) ();
  let button_h_box = GPack.hbox ~packing:top_v_box#pack () in
  let dismiss_button = 
    GButton.button ~label:"Dismiss" ~packing:button_h_box#pack ()
  in
  let sticky_button = 
    GButton.toggle_button ~label:"Sticky" 
      ~packing:(button_h_box#pack ~from:`END) ()
  in
  let node_window = 
    new node_window proof_window node top_window text_win
      sticky_button window_number 
  in
  let title_start = match node#node_kind with
    | Proof_command -> "Tactic "
    | Turnstile -> "Sequent "
  in
  top_window#set_title (title_start ^ window_number ^ " of " ^ proof_name);
  ignore(top_window#event#connect#key_press node_window#key_pressed_callback);
  text_win#buffer#set_text node#content;

  ignore(top_window#connect#destroy 
	   ~callback:node_window#delete_node_window);
  ignore(dismiss_button#connect#clicked 
	   ~callback:node_window#delete_node_window);
  top_window#show ();

  node_window
