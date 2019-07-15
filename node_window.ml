(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2016 Hendrik Tews
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


(** Creation, display and drawing of the extra node windows *)


open Util
open Configuration
open Draw_tree


(** State class for external node windows. Objects of this class
    represent external windows for proof commands and sequents
    internally. They contain all the necessary state and methods for
    these windows. The creation of the window and the object as well
    signal connection happens outside in {!make_node_window}.
*)
class node_window proof_window node top_window text_window 
  detach_button (version_label : GMisc.label option)
  sequent_history window_number proof_name =
object (self)

  (** Flag whether this window is orphaned. An orphaned window has no
      connection to the proof tree anymore.
  *)
  val mutable orphaned = false

  (** Link to the proof window. Used to keep
      {!Proof_window.proof_window.node_windows} up to date and to
      redraw the tree when a node window is deleted. This link gets
      deleted when this node window becomes orphaned.
  *)
  val mutable proof_window = Some proof_window

  (** Link to the proof-tree element. Used to keep
      {!Draw_tree.proof_tree_element.external_windows} up to date.
      This link is deleted when this node becomes orphaned.
  *)
  val mutable node = Some node

  (** Local copy of the sequent text history. Used for the feature
      that sticky node windows don't change their display and history.
      For proof commands, this holds always just the current proof
      command.
  *)
  val mutable sequent_history = sequent_history

  (** The element of the sequent history that is currently on display.
      This counts forwards, starting with 0. 0 means the current
      version. For proof-command windows, this is always 0, to always
      select the only element of the history list.
  *)
  val mutable sequent_history_pos = 0

  (** Number of this node window. Used to correlate node windows with
      the proof-tree display.
  *)
  method window_number = window_number

  (** Make this node window orphaned. A orphaned node window is not
      connected with the proof tree any more. Its Sticky button is 
      disabled.
  *)
  method private orphan_node_window =
    if not orphaned then begin
      (match node with
	| Some node -> 
	  node#delete_external_window (self :> external_node_window);
	  top_window#set_title 
	    ((match node#node_kind with
	      | Turnstile -> "Orphaned sequent of "
	      | Proof_command -> "Orphaned tactic of "
	     ) ^ proof_name)
	| None -> assert false
      );
      (match proof_window with
	| Some proof_window -> 
	  proof_window#delete_node_window self;
	  proof_window#invalidate_drawing_area;
	| None -> assert false
      );
      detach_button#misc#set_sensitive false;
      node <- None;
      proof_window <- None;
      orphaned <- true;
    end
    

  (** Delete and destroy this node window. *)
  method delete_node_window () =
    self#orphan_node_window;
    top_window#destroy()

  (** Key event callback for deleting and destroying this node window.
      Returns [true] to indicate that the key has been processed.
  *)
  method private delete_node_window_event _ =
    self#delete_node_window ();
    true

  (** Delete this node window if it is not detached. Needs to be called
      when the corresponding element in the proof-tree display is
      deleted.
  *)
  method delete_attached_node_window =
    if not detach_button#active 
    then self#delete_node_window ()
    else self#orphan_node_window

  (** Callback for key events. Deals only with 'Q' and 'q'. *)
  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
      | ks when (ks = GdkKeysyms._Q || ks = GdkKeysyms._q)  -> 
	self#delete_node_window_event ev

      | _ -> false

  (** Reconfigure and redraw the node window. Needs to be called when
      the configuration has been changed. Actually only the font of
      the buffer text is changed.
  *)
  method configuration_updated =
    text_window#misc#modify_font !sequent_font_desc;
    GtkBase.Widget.queue_draw top_window#as_widget

  (** Display the element from {!sequent_history} that
      {!sequent_history_pos} denotes. Update the history label.
  *)
  method private display_content =
    text_window#buffer#set_text
      (match sequent_history with
	| [] -> "waiting for sequent text"
	| _ -> List.nth sequent_history sequent_history_pos);
    (match version_label with
      | None -> ()
      | Some label ->
	let sequent_history_len = List.length sequent_history in
	label#set_label 
	  (Printf.sprintf "version %d of %d versions"
	     (sequent_history_len - sequent_history_pos) sequent_history_len)
    )

  (** Update the content in the text buffer of this node window. The
      argument tells how much the history length grew (greater zero)
      or shrunk (lesser zero).
  *)
  method update_content new_history =
    if not detach_button#active then begin
      if sequent_history_pos > 0 then begin
	let old_length = List.length sequent_history in
	let new_length = List.length new_history in
	sequent_history_pos <- max 0
	  (new_length - (old_length - sequent_history_pos))
      end;
      sequent_history <- new_history;
      self#display_content
    end

  (** Callback for the sequent-history-backwards button. *)
  method history_back_click () =
    sequent_history_pos <- 
      min (List.length sequent_history - 1) (sequent_history_pos + 1);
    self#display_content


  (** Callback for the sequent-history-forwards button. *)
  method history_for_click () =
    sequent_history_pos <- max 0 (sequent_history_pos - 1);
    self#display_content

  (** Callback for  *)
  method detach_button_toggle () =
    if not detach_button#active then
      match node with
	| None -> assert false
	| Some sn -> self#update_content sn#sequent_text_history

end


(** Create and initialize a new external node window. Composes the GTK
    window, fills the initial content, creates the contolling object and
    connects hooks and signals.
*)
let make_node_window proof_window proof_name node window_number =
  let top_window = GWindow.window () in
  let top_v_box = GPack.vbox ~packing:top_window#add () in
  let scrolling = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(top_v_box#pack ~expand:true) ()
  in
  let text_win = 
    GText.view ~editable:false ~cursor_visible:false
      ~packing:scrolling#add ()
  in
  text_win#misc#modify_font !sequent_font_desc;
  let context = text_win#misc#pango_context in
  let metrics = context#get_metrics() in
  let char_width = GPango.to_pixels(metrics#approx_char_width) in
  let layout = context#create_layout in
  Pango.Layout.set_text layout "X";
  let (_, char_height) = Pango.Layout.get_pixel_size layout in
  let lines = 
    min (Util.number_of_lines node#displayed_text) 
      !current_config.node_window_max_lines
  in
  (* 
   * text_win#misc#set_size_request 
   *   ~width:(char_width * 60) ~height:(char_height * lines) ();
   *)
  (* text_win#misc#set_size_chars ~width:60 ~height:lines (); *)
  top_window#set_default_size ~width:(char_width * 80) 
    ~height:(char_height * (lines + 2));
  let button_h_box = GPack.hbox ~spacing:5 ~packing:top_v_box#pack () in
  let dismiss_button = 
    GButton.button ~stock:`CLOSE ~packing:(button_h_box#pack ~from:`END) ()
  in
  let detach_button = 			(* XXX find stock item *)
    GButton.toggle_button ~label:"Detach" ~packing:button_h_box#pack ()
  in
  let sequent_history = node#sequent_text_history in
  let (version_back_button, version_label, version_for_button) = 
    match node#node_kind with
      | Proof_command -> (None, None, None)
      | Turnstile ->
	let version_back_button = 
	  GButton.button ~stock:`GO_BACK ~packing:button_h_box#pack ()
	in
	let sequent_history_len = List.length sequent_history in
	let version_label =
	  GMisc.label 
	    ~text:(Printf.sprintf "version %d of %d versions"
		     sequent_history_len sequent_history_len)
	    ~packing:button_h_box#pack ()
	in
	let version_for_button = 
	  GButton.button ~stock:`GO_FORWARD ~packing:button_h_box#pack ()
	in
	(Some version_back_button, Some version_label, Some version_for_button)
  in
  let node_window = 
    new node_window proof_window node top_window text_win
      detach_button version_label sequent_history window_number proof_name
  in
  node#register_external_window (node_window :> external_node_window);

  let title_start = match node#node_kind with
    | Proof_command -> "Tactic "
    | Turnstile -> "Sequent "
  in
  top_window#set_title (title_start ^ window_number ^ " of " ^ proof_name);
  ignore(top_window#event#connect#key_press 
	   ~callback:node_window#key_pressed_callback);
  text_win#buffer#set_text node#displayed_text;

  ignore(detach_button#connect#toggled
	   ~callback:node_window#detach_button_toggle);
  (match node#node_kind with
    | Proof_command -> ()
    | Turnstile ->
      ignore((access_option version_back_button)#connect#clicked
		~callback:node_window#history_back_click);
      ignore((access_option version_for_button)#connect#clicked
		~callback:node_window#history_for_click);
  );

  ignore(top_window#connect#destroy 
	   ~callback:node_window#delete_node_window);
  ignore(dismiss_button#connect#clicked 
	   ~callback:node_window#delete_node_window);
  top_window#show ();

  node_window
