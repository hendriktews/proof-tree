(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2013 Hendrik Tews
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
 * $Id: proof_window.ml,v 1.64 2013/08/04 22:21:33 tews Exp $
 *)


(** Creation, display and drawing of the main proof-tree window *)


(* open Util *)
open Gtk_ext
open Configuration
open Emacs_commands
open Draw_tree
open Tree_layers
open Node_window
open Help_window
open About_window
open Ext_dialog


(** Callback for higher-level modules when the user deletes a proof
    window. During start-up the reference is changed to
    {!Proof_tree.quit_proof}.
*)
let delete_proof_tree_callback = ref (fun (_ : string) -> ())


(** Contains proof window clones. See also
    [Proof_tree.original_proof_trees].
*)
let cloned_proof_windows = ref []

(** Class for managing the main proof-tree window. Contains methods
    for all callbacks and the necessary state. The widget tree the
    binding to singals must be done outside.

    Arguments are
    - top_window              {xref lablgtk class GWindow.window} 
                              of the top-level widget
    - drawing_h_adjustment    {xref lablgtk class GData.adjustment}
                              of the horizontal scroll bar
                              of the drawing area
    - drawing_v_adjustment    {xref lablgtk class GData.adjustment}
                              of the vertical scroll bar
                              of the drawing area
    - drawing_area            {xref lablgtk class GMisc.drawing_area}
                              for the proof tree
    - drawable                {!Gtk_ext.better_drawable} for the actual 
                              drawing operations
    - labeled_sequent_frame   {xref lablgtk class GBin.frame}
                              labelled frame of the sequent area
    - sequent_window          {xref lablgtk class GText.view}
                              of the sequent area
    - sequent_v_adjustment    {xref lablgtk class GData.adjustment}
                              of the vertical scroll bar of the sequent area
    - message_label           {xref lablgtk class GMisc.label}
                              for messages in the bottom line
    - context_menu            {xref lablgtk class GMenu.menu} 
                              of the context menu
    - proof_name              the name of the proof this display is showing
*)
class proof_window (top_window : GWindow.window)
  drawing_h_adjustment drawing_v_adjustment (drawing_area : GMisc.drawing_area)
  (drawable : better_drawable)
  labeled_sequent_frame sequent_window sequent_v_adjustment
  (message_label : GMisc.label) context_menu proof_name
  =
object (self)

  (***************************************************************************
   *
   * Internal state and setters/accessors
   *
   ***************************************************************************)

  (** x-offset of the bounding box of the complete proof tree layer
      stack. This is only non-zero when the width of the complete
      layer stack is smaller than the width of the drawing area. *)
  val mutable top_left = 0

  (** y-offset of the bounding box of the top layer. Constantly 0. *)
  val top_top = 0

  (** [true] if the sequent area should always show the last line.
      This is set to [true] when a sequent is shown and to false when
      a proof command is shown. *)
  val mutable sequent_window_scroll_to_bottom = false

  (** The stack of layers of proof trees *)
  val layer_stack = new tree_layer_stack

  (** The current node of the proof tree, if there is one *)
  val mutable current_node = None

  (** Cache holding the relative bounding box of the current node. If
      not [None], it holds the tuple [(x_low, x_high, y_low, y_high)]
      relative to the top-left corner (i.e., ({!top_left},
      {!top_top})) of the bounding box of the complete proof tree.
  *)
  val mutable current_node_offset_cache = None

  (** Set to [true] when we still have to reposition the drawing area
      to make the current node visible. This is needed because of the
      asynchronous nature of GTK, which does not immediately resize
      the drawing area when I request it. Therefore the positioning
      code has to wait until the drawing area has changed its size and
      all needed scrollbars have been added.
  *)
  val mutable position_to_current_node = true

  (** List of position hints, containing lists of proof tree nodes. If
      possible, the current node will positioned such that some hint
      nodes are also visible. If the list is longer than one, hints
      are tried one after each other. 
  *)
  val mutable position_hints = []

  (** Holds the selected node, if there is one. *)
  val mutable selected_node = None

  (** List of all external non-orphaned node windows that belong to nodes 
      in the current proof tree. This list is stored only for optimization.
  *)
  val mutable node_windows = []

  (** The management object for the dialog for existential
      variables, if present. 
  *)
  val mutable existential_window = None

  (** When we are asked to destroy this proof-tree window (because of
      an undo or because Proof General sent a quit-proof command) then
      the destruction of the top-level GTK window will emit a destroy
      signal that we interpret as a user destroy button click.
      Further, the deletion of this proof from the various list with
      proof_tree structures, might cause a second call of the
      {!delete_proof_window} method. This flag is used to avoid this
      double or triple killing.
  *)
  val mutable destroy_in_progress = false

  (** True when this is a clone. *)
  val mutable is_clone = false

  (** A proof is disconnected if it is not the current proof that is
      currently updated by Proof General. Used for the context menu.
  *)
  val mutable disconnected = false

  (** The stack of layers, containing all proof trees belonging to
      this proof.
  *)
  method layer_stack = layer_stack

  (** Return the selected node or [None] if there is none. *)
  method get_selected_node = selected_node

  (** Delete the external node window from {!node_windows}. *)
  method delete_node_window win =
    node_windows <- List.filter (fun owin -> owin <> win) node_windows

  (** Setter for {!is_clone}. *)
  method set_clone_flag = is_clone <- true

  (** Setter for {!position_hints}. *)
  method set_position_hints hints =
    position_hints <- (hints : proof_tree_element list list)

  (** Clear {!position_hints}. *)
  method clear_position_hints = position_hints <- []

  (***************************************************************************
   *
   * Messages
   *
   ***************************************************************************)

  (** Display text in the message label. *)
  method message text = message_label#set_label text

  (***************************************************************************
   *
   * Sequent window
   *
   ***************************************************************************)

  (** Make the last line visible in the sequent area, if
      {!sequent_window_scroll_to_bottom} is [true].

      This is the callback for the changed signal of the vertical
      adjustment of the scrollbar of the sequent area (emitted when
      the sequent area changes its size but not when the scrollbar is
      moved).
  *)
  method sequent_area_changed () =
    if sequent_window_scroll_to_bottom then
      let a = sequent_v_adjustment in
      a#set_value (max a#lower (a#upper -. a#page_size))

  (** [update_sequent_area label content scroll_to_bottom] updates the
      sequent area to show [content] with label [label] on the frame.
      It further sets {!sequent_window_scroll_to_bottom} to
      [scroll_to_bottom].
  *)
  method private update_sequent_area label content scroll_to_bottom =
    labeled_sequent_frame#set_label (Some label);
    sequent_window#buffer#set_text content;
    sequent_window_scroll_to_bottom <- scroll_to_bottom

  (** Clear the sequent area. *)
  method private clear_sequent_area =
    self#update_sequent_area "no sequent" "" false

  (** Refresh the sequent area. If there is a selected node, show it.
      Otherwise, if there is a current sequent node that has a parent
      sequent, then show the parent sequent. Otherwise clear the sequent
      area.
  *)
  method refresh_sequent_area =
    match selected_node with
      | Some node ->
	let (frame_text, scroll_to_bottom) = match node#node_kind with
	  | Turnstile -> ("Selected sequent", true)
	  | Proof_command -> ("Selected command", false)
	in
	self#update_sequent_area frame_text node#displayed_text scroll_to_bottom
      | None -> 
	match current_node with
	  | None -> self#clear_sequent_area
	  | Some node ->
	    if node#node_kind = Turnstile
	    then
	      match node#parent with
		| None -> self#clear_sequent_area
		| Some p -> match p#parent with
		    | None -> self#clear_sequent_area
		    | Some p ->
		      self#update_sequent_area
			"Previous sequent"
			p#displayed_text
			true
	    else
	      self#clear_sequent_area


  (***************************************************************************
   *
   * Current node
   *
   ***************************************************************************)

  (** Set the current node. *)
  method set_current_node n =
    current_node_offset_cache <- None;
    current_node <- Some (n : proof_tree_element)

  (** Clear the current node. *)
  method clear_current_node =
    current_node_offset_cache <- None;
    current_node <- None;


  (***************************************************************************
   *
   * Unclassified methods
   *
   ***************************************************************************)

  (** Tell whether this proof-tree window should stay alive when the
      user retracted to a point before the start of the proof. If the
      window shows more than just the root sequent, it will stay
      alive.
  *)
  method survive_undo_before_start = 
    layer_stack#survive_undo_before_start_hint

  (** Disconnect the proof tree. Clears all current node and current
      branch indications.
  *)
  method disconnect_proof = 
    disconnected <- true;
    layer_stack#disconnect

  (** Reflect changes in the current configuration. *)
  method configuration_updated =
    sequent_window#misc#modify_font !sequent_font_desc;
    drawable#set_line_attributes 
      ~width:(!current_config.turnstile_line_width) ();
    layer_stack#configuration_updated;
    self#expand_drawing_area;
    ignore(self#position_tree);
    GtkBase.Widget.queue_draw top_window#as_widget;
    self#configuration_updated_ext_dialog
      
  (** Clear this window and the existential variable dialog, if there
      is one, for reuse when the proof it displays is started again.
      Deletes and destroys all non-sticky external node windows.
  *)
  method clear_for_reuse =
    disconnected <- false;
    layer_stack#clear_for_reuse;
    current_node <- None;
    current_node_offset_cache <- None;
    selected_node <- None;
    self#refresh_sequent_area;
    List.iter (fun w -> w#delete_non_sticky_node_window) node_windows;
    assert(node_windows = []);
    self#clear_existential_dialog_for_reuse

  (** Update the existentials info in external sequent displays. *)
  method update_sequent_existentials_info =
    layer_stack#update_sequent_existentials_info

  (** Find the first node in the proof tree satisfying the predicate
      using depth-first search.
  *)
  method find_node p = layer_stack#find_node p


  (***************************************************************************
   *
   * Position to nodes and points
   *
   ***************************************************************************)

  (** Make node visible. *)
  method private clamp_to_node node =
    let (x_l, x_u, y_l, y_u) = node#bounding_box top_left top_top in
    drawing_h_adjustment#clamp_page ~lower:x_l ~upper:x_u;
    drawing_v_adjustment#clamp_page ~lower:y_l ~upper:y_u


  (** Move the drawing area to show point [(x,y)] as centre point. *)
  method private centre_point x y =
    let x_size = drawing_h_adjustment#page_size in
    let adj_x_l = drawing_h_adjustment#lower in
    let adj_x_u = drawing_h_adjustment#upper in
    let x = x -. x_size /. 2.0 in
    let x = if x +. x_size > adj_x_u then adj_x_u -. x_size else x in
    drawing_h_adjustment#set_value (max adj_x_l x);
    let y_size = drawing_v_adjustment#page_size in
    let adj_y_l = drawing_v_adjustment#lower in
    let adj_y_u = drawing_v_adjustment#upper in
    let y = y -. y_size /. 2.0 in
    let y = if y +. y_size > adj_y_u then adj_y_u -. y_size else y in
    drawing_v_adjustment#set_value (max adj_y_l y)

  (** Show the given node in the centre of the drawing area. *)
  method private centre_node node =
    let (x_l, x_u, y_l, y_u) = node#bounding_box top_left top_top in
    self#centre_point ((x_l +. x_u) /. 2.0) ((y_l +. y_u) /. 2.0)

  (** Test if the given node is (partially) visible. *)
  method private is_partially_visible node =
    let (node_x_l, node_x_u, node_y_l, node_y_u) = 
      node#bounding_box top_left top_top in
    let in_x = inside_adj_range drawing_h_adjustment in
    let in_y = inside_adj_range drawing_v_adjustment
    in
    ((in_x node_x_l) || (in_x node_x_u))
    && ((in_y node_y_l) || (in_y node_y_u))

  (** Make the given node visible in a user friendly way. *)
  method show_node node =
    if self#is_partially_visible node 
    then self#clamp_to_node node
    else self#centre_node node

  (** Make the two given nodes visible in a user friendly way. If both
      nodes do not fit together in the drawing area, then only the second
      node is shown.
  *)
  method show_both_nodes n1 n2 =
    let (x1_l, x1_u, y1_l, y1_u) = n1#bounding_box top_left top_top in
    let (x2_l, x2_u, y2_l, y2_u) = n2#bounding_box top_left top_top in
    let x_l = min x1_l x2_l in
    let x_u = max x1_u x2_u in
    let y_l = min y1_l y2_l in
    let y_u = max y1_u y2_u in
    (* 
     * Printf.fprintf (debugc()) 
     *   "SBN n1 x %.0f-%.0f y %.0f-%.0f  "
     *   "n2 x %.0f-%.0f y %.0f-%.0f bb x %.0f-%.0f y %.0f-%.0f\n%!"
     *   x1_l x1_u y1_l y1_u
     *   x2_l x2_u y2_l y2_u
     *   x_l x_u y_l y_u;
     *)
    if x_u -. x_l <= drawing_h_adjustment#page_size
      && y_u -. y_l <= drawing_v_adjustment#page_size
    then
      (* both n1 and n2 fit on the page *)
      if self#is_partially_visible n1 || self#is_partially_visible n2
      then begin
	self#clamp_to_node n1;
	self#clamp_to_node n2
      end
      else
	self#centre_point ((x_l +. x_u) /. 2.0) ((y_l +. y_u) /. 2.0)
    else
      self#show_node n2


  (***************************************************************************
   *
   * Key events
   *
   ***************************************************************************)

  (** Delete and destroy this proof-tree window, including all its
      non-sticky external node windows and the existential variable
      dialog. Delete this window from {!cloned_proof_windows}. Other
      data structures in higher-level modules are not touched.

      The destruction process might cause destroy signals which will
      cause this method to be called again. The flag
      {!destroy_in_progress} is used to protect against such cases.
  *)
  method delete_proof_window =
    if destroy_in_progress = false then begin
      destroy_in_progress <- true;
      List.iter (fun w -> w#delete_non_sticky_node_window) node_windows;
      self#destroy_existential_dialog;
      let self = (self :> proof_window) in
      cloned_proof_windows :=
	List.fold_left 
        (fun res w -> if w = self then res else w :: res)
        [] !cloned_proof_windows;
      top_window#destroy()
    end

  (** Callback for the "Dismiss" button and the destroy signal
      (emitted, for instance, when the window manager kills this
      window). Delete and destroy this window as for
      {!delete_proof_window} and delete it from all data structures in
      the module {!Proof_tree}.
  *)
  method user_delete_proof_window () =
    if destroy_in_progress = false then begin
      if is_clone = false then
	!delete_proof_tree_callback proof_name;
      self#delete_proof_window
    end

  (** Callback for key events. Call the appropriate action for each
      key.
  *)
  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
      | ks when 
	  (ks = GdkKeysyms._Q or ks = GdkKeysyms._q) 
	  && (List.mem `CONTROL (GdkEvent.Key.state ev))
	  -> 
	exit 0
      | ks when (ks = GdkKeysyms._Q or ks = GdkKeysyms._q)  -> 
	self#user_delete_proof_window (); true
      | ks when ks = GdkKeysyms._Left -> 
	scroll_adjustment drawing_h_adjustment (-1); true
      | ks when ks = GdkKeysyms._Right -> 
	scroll_adjustment drawing_h_adjustment 1; true
      | ks when ks = GdkKeysyms._Up -> 
	scroll_adjustment drawing_v_adjustment (-1); true
      | ks when ks = GdkKeysyms._Down -> 
	scroll_adjustment drawing_v_adjustment 1; true

      | ks when (ks = GdkKeysyms._E or ks = GdkKeysyms._e)  -> 
      	self#show_existential_window (); true

      (* 
       * | ks when (ks = GdkKeysyms._C or ks = GdkKeysyms._c)  -> 
       * 	show_config_window (); true
       *)

      | _ -> false


  (***************************************************************************
   *
   * Window for existential variables
   *
   ***************************************************************************)

  (** Clear {!existential_window}. *)
  method existential_clear () = 
    existential_window <- None

  (** Show a dialog for existential variables. If there is currently
      none, a new one is created and initialized.
  *)
  method show_existential_window () =
    match existential_window with
      | Some w -> w#present
      | None -> 
	let ext = 
	  make_ext_dialog (self :> proof_window) proof_name 
	in
	layer_stack#init_ext_dialog ext;
	ext#finish_table_init;
	existential_window <- Some ext

  (** Destroy the existential variables dialog, if one is present. *)
  method private destroy_existential_dialog =
    match existential_window with
      | None -> ()
      | Some w -> 
	w#destroy ();
	existential_window <- None

  (** Prepare the existential variable dialog for reuse, if there is
      one.
  *)
  method private clear_existential_dialog_for_reuse =
    match existential_window with
      | None -> ()
      | Some w -> w#clear_for_reuse

  (** Schedule some existentials to be added to the existential
      variable dialog, if there is one.
  *)
  method ext_dialog_add new_ext =
    match existential_window with
      | Some w -> w#change_and_add new_ext
      | None -> ()

  (** Schedule some existentials to be removed from the existential
      variable dialog, if there is one.
  *)
  method ext_dialog_undo remove_ext =
    match existential_window with
      | Some w -> w#change_and_delete remove_ext
      | None -> ()

  (** Process all pending requests for the existential variable
      dialog, if there is one.
  *)
  method update_ext_dialog =
    match existential_window with
      | Some w -> w#update_ext_dialog
      | None -> ()

  (** Ask the existential variable dialog to reflect changes in the
      current configuration.
  *)
  method private configuration_updated_ext_dialog =
    match existential_window with
      | Some w -> w#configuration_updated
      | None -> ()

  (***************************************************************************
   *
   * Redraw / expose events
   *
   ***************************************************************************)

  (** Return the relative bounding box of the current node, see
      {!current_node_offset_cache}. If there nothing in the cache, try
      to fill it. Return [None] if there is no current node.
  *)
  method private get_current_offset =
    match current_node_offset_cache with
      | Some _ as res -> res
      | None -> match current_node with
	  | None -> None
	  | Some node ->
	    let res = Some node#bounding_box_offsets in
	    current_node_offset_cache <- res;
	    res

  (** Erase the complete drawing area. *)
  method private erase = 
    (* Printf.fprintf (debugc()) "ERASE\n%!"; *)
    let (x,y) = drawable#size in
    let gc = save_gc drawable in
    let bg = top_window#misc#style#bg `PRELIGHT in
    (* let bg = drawable#get_background in *)
    (* drawable#set_foreground (`NAME("gray85")); *)
    drawable#set_foreground (`COLOR bg);
    drawable#polygon ~filled:true [(0,0); (x,0); (x,y); (0,y)];
    restore_gc drawable gc

  (** Try to change the scrollbars of the drawing area such that the
      current node becomes visible in the bottom part. Do nothing if
      {!position_to_current_node} is [false]. Making the current node
      visible might be impossible, because the resize request for the
      drawing area might not have been processed yet. In this case
      {!position_to_current_node} stays true and this method is called
      again at some later stage.
  *)
  method private try_adjustment = 
    if position_to_current_node = true then
      match self#get_current_offset with
	| None -> 
	  position_to_current_node <- false
	| Some((x_l_off, x_u_off, y_l_off, y_u_off)) ->
	  let x_page_size = int_of_float drawing_h_adjustment#page_size in
	  let y_page_size = int_of_float drawing_v_adjustment#page_size in
	  let x_l_f = float_of_int(top_left + x_l_off) in
	  let x_u_f = float_of_int(top_left + x_u_off) in
	  let y_l_f = float_of_int(top_top + y_l_off) in
	  let y_u_i = top_top + y_u_off in
	  let y_u_f = float_of_int y_u_i in
	  (* 
           * Printf.fprintf (debugc()) 
	   *   "TRY ADJUSTMENT x %.1f-%.1f y %.1f-%.1f\n%!"
	   *   x_l_f x_u_f y_l_f y_u_f;
           *)
	  let success = ref true in
          (* The following code might immediately trigger
	   * expose events, which will call try_adjustment again. To avoid
	   * entering this function a second time before leaving it, I
	   * temporarily switch position_to_current_node off.
	   *)
	  position_to_current_node <- false;

	  if x_page_size >= (x_u_off - x_l_off) && 
	    y_page_size >= (y_u_off - y_l_off)
	  then begin
	    (* current node fits into the viewport, be sophisticated *)
	    let req_width = layer_stack#width in
	    let req_height = layer_stack#height in
	    if (float_of_int req_width) > drawing_h_adjustment#upper ||
	       (float_of_int req_height) > drawing_v_adjustment#upper
	    then begin
	      (* The resize request for the drawing area has not 
	       * been processed yet. It might happen that this resize
	       * request causes the addition of some scrollbars. In
	       * this case the viewport gets smaller and the 
	       * current node would possible (partially) hidden.
	       * Therefore we mimic an adjustment error. Note that 
	       * in this case also clamp_page would not give a proper
	       * adjustment.
	       *)
	      success := false;
	      (* Printf.fprintf (debugc()) "clever forced error %!" *)
	    end else begin
	      let y_val = 
		max drawing_v_adjustment#lower 
		  (float_of_int (y_u_i - y_page_size))
	      in
	      drawing_v_adjustment#set_value y_val;

	      (* make now the _h_adjustment, first try if one of the 
               * hint nodes can be made visible as well
	       *)
	      if 
		List.exists
		  (fun hint_nodes ->
		    let (off_l, off_u) =
		      List.fold_left
			(fun (off_l, off_u) hint_node ->
			  let (h_x_l_off, h_x_u_off, _, _) = 
			    hint_node#bounding_box_offsets 
			  in
			  (min off_l h_x_l_off, max off_u h_x_u_off))
			(x_l_off, x_u_off)
			hint_nodes
		    in
		    if x_page_size >= (off_u - off_l)
		    then begin
		      (* the hints fit *)
		      drawing_h_adjustment#clamp_page
			~lower:(float_of_int (top_left + off_l))
			~upper:(float_of_int (top_top + off_u));
		      true
		    end else 
		      (* the hints don't fit *)
		      false
		  )
		  position_hints
	      then ()
	      else
		drawing_h_adjustment#clamp_page ~lower:x_l_f ~upper:x_u_f;
	      (* 
               * Printf.fprintf (debugc()) "clever y_u_i %d up %d y_val %d %!"
	       * 	y_u_i
	       * 	(int_of_float drawing_v_adjustment#upper)
	       * 	(int_of_float y_val);
               *)
	    end
	  end else begin
	    (* very small viewport, use dump strategy *)
	    (* Printf.fprintf (debugc()) "dump clamp %!"; *)
	    drawing_h_adjustment#clamp_page ~lower:x_l_f ~upper:x_u_f;
	    drawing_v_adjustment#clamp_page ~lower:y_l_f ~upper:y_u_f;
	  end;

	  if !success && 
	    range_inside_adj_range drawing_h_adjustment x_l_f x_u_f &&
	    range_inside_adj_range drawing_v_adjustment y_l_f y_u_f
	  then begin
	    (* Printf.fprintf (debugc()) "SUCCESSFUL\n%!"; *)
	    () (* Do nothing: leave position_to_current_node disabled *)
	  end else begin
	    (* Printf.fprintf (debugc()) "UNSUCCESSFUL %b\n%!" !success; *)
	    (* Schedule the adjustment again, hope that we are more
	     * successful next time.
	     *)
	    position_to_current_node <- true;
	  end;
          (* 
           * (let a = drawing_h_adjustment in
	   *  Printf.fprintf (debugc()) 
	   *    ("TA HADJ low %.1f val %.1f " ^^ 
	   * 	 "up %.1f size %.1f step %.1f page %.1f\n%!")
	   *    a#lower a#value a#upper a#page_size 
	   *    a#step_increment a#page_increment);
           * (let a = drawing_v_adjustment in
	   *  Printf.fprintf (debugc()) 
	   *    ("TA VADJ low %.1f val %.1f " ^^
	   * 	 "up %.1f size %.1f step %.1f page %.1f\n%!")
	   *    a#lower a#value a#upper a#page_size 
	   *    a#step_increment a#page_increment);
           *)
	  ()

  (** Request the drawing are to change its size to the size of the
      current proof tree.
  *)
  method private expand_drawing_area =
    let new_width = layer_stack#width in
    let new_height = layer_stack#height in
    (* 
     * Printf.fprintf (debugc()) "DRAWING AREA SIZE REQUEST %d x %d\n%!" 
     *   new_width new_height;
     *)
    (* 
     * if new_width > current_width || new_height > current_height then
     *   drawing_area#misc#set_size_request
     * 	~width:(max current_width new_width)
     * 	~height:(max current_height new_height) ();
     *)
    drawing_area#misc#set_size_request ~width:new_width ~height:new_height ();

  (** Sets the position of the proof tree in the drawing area by 
      computing [top_left]. Returns true if the position changed. 
      In that case the complete drawing area must be redrawn.
  *)
  method private position_tree =
    let old_top_left = top_left in
    let (width, _) = drawable#size in
    top_left <- max 0 ((width - layer_stack#width) / 2);
    top_left <> old_top_left

  (** Handle expose events. Try to move to the current node, erase and
      redraw the complete drawing area.
  *)
  method private redraw =
    (* 
     * (let a = drawing_v_adjustment in
     *  Printf.fprintf (debugc()) 
     *    "RD %s VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    (match scheduled_adjustment with | None -> "N" | Some _ -> "S")
     *    a#lower a#value a#upper a#page_size 
     *    a#step_increment a#page_increment);
     *)
    self#try_adjustment;
    self#erase;
    (* Printf.fprintf (debugc()) "REDRAW\n%!"; *)
    layer_stack#draw top_left top_top

  (** Schedule an expose event for the drawing area, thereby causing
      it to get redrawn.
  *)
  method invalidate_drawing_area =
    (* Printf.fprintf (debugc()) "INVALIDATE\n%!"; *)
    GtkBase.Widget.queue_draw drawing_area#as_widget

  (** Method for updating the display after the proof tree has changed.
      Adjusts the tree position in the drawing area, schedules a 
      complete redraw and make the current node (if any) visible.
  *)
  method refresh_and_position =
    (* Printf.fprintf (debugc()) "REFRESH & POSITION\n%!"; *)
    position_to_current_node <- true;
    self#expand_drawing_area;
    ignore(self#position_tree);
    self#try_adjustment;
    self#invalidate_drawing_area;
    (* Printf.fprintf (debugc()) "REFRESH & POSITION END\n%!"; *)
    ()

  (** Position the current node in the bottom part of the drawing
      area.
  *)
  method reposition_current_node () =
    position_to_current_node <- true;
    self#try_adjustment

  (** Make the current node visible in a user friendly way. *)
  method show_current_node () =
    match current_node with
      | None -> ()
      | Some current ->
	if self#is_partially_visible current
	then self#show_node current
	else self#reposition_current_node ()

  (** Make the selected node visible in a user friendly way. *)
  method show_selected_node () =
    match selected_node with
      | None -> ()
      | Some selected -> self#show_node selected

  (** Callback for the size_allocate signal of the drawing area.
      Position the proof tree in the viewport of the drawing area and
      redraw.
  *)
  method draw_scroll_size_allocate_callback (_size : Gtk.rectangle) =
    (* 
     * Printf.fprintf (debugc()) "SCROLLING SIZE ALLOC SIGNAL size %d x %d\n%!"
     *   (int_of_float (drawing_h_adjustment#upper +. 0.5))
     *   (int_of_float (drawing_v_adjustment#upper +. 0.5));
     *)
    let need_redraw = self#position_tree in
    (* 
     * (let a = drawing_v_adjustment in
     *  Printf.fprintf (debugc()) 
     *    "SA %s VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    (match scheduled_adjustment with | None -> "N" | Some _ -> "S")
     *    a#lower a#value a#upper a#page_size 
     *    a#step_increment a#page_increment);
     *)
    self#try_adjustment;
    if need_redraw 
    then self#invalidate_drawing_area

  (* 
   * method draw_area_size_allocate_callback (_size : Gtk.rectangle) =
   *   Printf.fprintf (debugc()) "AREA SIZE ALLOC SIGNAL size %d x %d\n%!"
   *     (int_of_float (drawing_h_adjustment#upper +. 0.5))
   *     (int_of_float (drawing_v_adjustment#upper +. 0.5));
   *)
  
  (* 
   * method draw_area_configure_callback configure_event =
   *   Printf.fprintf (debugc()) 
   *     "AREA CONFIGURE SIGNAL area size %d x %d scroll size %d x %d\n%!"
   *     (GdkEvent.Configure.width configure_event)
   *     (GdkEvent.Configure.height configure_event)
   *     (int_of_float (drawing_h_adjustment#upper +. 0.5))
   *     (int_of_float (drawing_v_adjustment#upper +. 0.5));
   *   false
   *)

  (** Callback for exposure events. Redraw the complete tree. *)
  method expose_callback (_ev : GdkEvent.Expose.t) =
    (* 
     * let r = GdkEvent.Expose.area ev in
     * Printf.fprintf (debugc()) "EXPOSE count %d %d x %d at %d x %d\n%!"
     *   (GdkEvent.Expose.count ev)
     *   (Gdk.Rectangle.width r) (Gdk.Rectangle.height r)
     *   (Gdk.Rectangle.x r) (Gdk.Rectangle.y r);
     *)
    (* 
     * (let a = drawing_v_adjustment in
     *  Printf.fprintf (debugc())
     * "EX VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    a#lower a#value a#upper a#page_size 
     *    a#step_increment a#page_increment);
     *)
    (* 
     * (let a = drawing_h_adjustment in
     *  Printf.fprintf (debugc())
     * "HADJ low %f val %f up %f size %f step %f page %f\n"
     *    a#lower a#value a#upper a#page_size a#step_increment a#page_increment);
     * (let a = drawing_v_adjustment in
     *  Printf.fprintf (debugc())
     * "VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    a#lower a#value a#upper a#page_size a#step_increment a#page_increment);
     *)
    self#redraw;
    (* prerr_endline "END EXPOSE EVENT"; *)
    false


  (***************************************************************************
   *
   * numbers for external node windows
   *
   ***************************************************************************)

  (** Counter for external node windows. *)
  val mutable last_node_number = 0

  (** Return the number for the next external node window. *)
  method private next_node_number =
    last_node_number <- last_node_number + 1;
    last_node_number


  (***************************************************************************
   *
   * Button events
   *
   ***************************************************************************)

  (** The dragging feature has the problem that it changes the
      viewport to the drawing area, which itself generates pointer
      motion events for the drawing area. The approach is therefore to
      remember the mouse position at the start of the dragging (i.e.,
      the button press) in coordinates relative to the complete X
      window.

      X position of the mouse at the last button press relative to
      [top_window]. 
  *)
  val mutable last_button_press_top_x = 0

  (** Y position of the mouse at the last button press relative to
      [top_window]. *)
  val mutable last_button_press_top_y = 0

  (** Value of the vertical scrollbar at the last button press. *)
  val mutable last_button_press_v_adjustment_value = 0.0

  (** Value of the horizontal scrollbar at the last button press. *)
  val mutable last_button_press_h_adjustment_value = 0.0

  (** Fill {!last_button_press_top_x}, {!last_button_press_top_y},
      {!last_button_press_v_adjustment_value} and
      {!last_button_press_h_adjustment_value} in case dragging
      follows.
  *)
  method private remember_for_dragging =
    let (x, y) = Gdk.Window.get_pointer_location top_window#misc#window in
    (* 
     * Printf.fprintf (debugc()) "Button press %d x %d\n%!" 
     *   (fst new_poi_loc) (snd new_poi_loc);
     *)
    last_button_press_top_x <- x;
    last_button_press_top_y <- y;
    last_button_press_v_adjustment_value <- drawing_v_adjustment#value;
    last_button_press_h_adjustment_value <- drawing_h_adjustment#value;


  (** We have different and incompatible actions for single and double
      clicks and for dragging. Because single click events do always occur
      before double clicks and dragging events, we have to undo the
      changes that were done for the single click. A double click is of
      course always preceeded by two single clicks. 

      Remember the selected node state before the last but one
      mouse press. This state needs to get reestablished when we
      detect a double click.
  *)
  val mutable old_old_selected_node = None

  (** Remember the selected node state before the last mouse press.
      This state needs to get reestablished on dragging events.
  *)
  val mutable old_selected_node = None

  (** Remember if we have already restored the selected node state
      since the last button press.
  *)
  val mutable restored_selected_node = false

  (** Reset the selected node state to state [node_opt] and update
      drawing and sequent area.
  *)
  method set_selected_node node_opt =
    (match selected_node with
      | None -> ()
      | Some node -> node#selected false);
    selected_node <- node_opt;
    (match node_opt with
      | None -> ()
      | Some node -> node#selected true);
    self#invalidate_drawing_area;
    self#refresh_sequent_area

  (** Make the root node the selected node or clear the selected node
      if there is no root node
  *)
  method select_root_node =
    self#set_selected_node (layer_stack#get_root_node)

  (** Save the selected node state on a button press and clear
      {!restored_selected_node}. 
  *)
  method private save_selected_node_state =
    old_old_selected_node <- old_selected_node;
    old_selected_node <- selected_node;
    restored_selected_node <- false

  (** Restore the selected node state when we detect mouse dragging
      for the first time. Sets {!restored_selected_node}.
  *)
  method private single_restore_selected_node =
    self#set_selected_node old_selected_node;
    restored_selected_node <- true

  (** Restore the selected node state after we detected a double
      click.
  *)
  method private double_restore_selected_node =
    self#set_selected_node old_old_selected_node;
    restored_selected_node <- true

  (** [locate_button_node x y found notfound] tries to locate a proof
      tree element on position [(x, y)]. If there is one [found] is called
      with this element. If there is nothing [notfound] is called.
  *)
  method private locate_button_node : 
    'a . int -> int -> (#proof_tree_element -> 'a) -> (unit -> 'a) -> 'a =
    fun x y node_click_fun outside_click_fun ->
      match layer_stack#find_node_for_point_in_layer_stack 
	                              top_left top_top x y with
	| None -> outside_click_fun ()
	| Some node -> node_click_fun node

  (** Display an external node window for the argument node. *)
  method private external_node_window (node : proof_tree_element) =
    let n = string_of_int(self#next_node_number) in
    let win = 
      make_node_window (self :> proof_window) proof_name node n 
    in 
    node_windows <- win :: node_windows;
    self#invalidate_drawing_area

  (** Method to handle button one press events. Receives as arguments
      the coordinates, a flag for the shift modifier and a flag to
      indicate a double click.
  *)
  method private button_1_press x y shifted double =
    self#remember_for_dragging;
    if (not double) && (not shifted)
    then self#save_selected_node_state;
    if double && (not shifted)
    then self#double_restore_selected_node;
    if double || shifted 
    then self#locate_button_node x y self#external_node_window (fun () -> ())
    else 
      self#locate_button_node x y 
	(fun node -> self#set_selected_node (Some node))
	(fun () -> self#set_selected_node None)

  (* val mutable last_button_press_time = 0l *)

  (** Generic call back for all button press events. *)
  method button_press ev =
    let x = int_of_float(GdkEvent.Button.x ev +. 0.5) in
    let y = int_of_float(GdkEvent.Button.y ev +. 0.5) in
    let button = GdkEvent.Button.button ev in
    let shifted = Gdk.Convert.test_modifier `SHIFT (GdkEvent.Button.state ev) in
    let double = match GdkEvent.get_type ev with
      | `BUTTON_PRESS -> false
      | `TWO_BUTTON_PRESS -> true
      | `THREE_BUTTON_PRESS -> false
      | `BUTTON_RELEASE -> false
    in
    (* 
     * let state = B.state ev in
     * let mod_list = Gdk.Convert.modifier state in
     * let _ = Gdk.Convert.test_modifier `SHIFT state in
     *)
    (* 
     * let time_diff = 
     * Int32.sub (GdkEvent.Button.time ev) last_button_press_time 
     * in
     * last_button_press_time <- GdkEvent.Button.time ev;
     *)
    (* 
     * (let h = drawing_h_adjustment in
     *  let v = drawing_v_adjustment in
     *  Printf.fprintf (debugc()) 
     *    "%s Button %s%d at %d x %d | x: %1.f - %.1f y: %.1f - %.1f\n%!" 
     *    (match GdkEvent.get_type ev with
     * 	 | `BUTTON_PRESS -> "single"
     * 	 | `TWO_BUTTON_PRESS -> "double"
     * 	 | `THREE_BUTTON_PRESS -> "triple"
     * 	 | `BUTTON_RELEASE -> "release")
     *    (if shifted then "shift " else "")
     *    button x y
     *    h#value (h#value +. h#page_size)
     *    v#value (v#value +. v#page_size));
     *)
    (match button with
      | 1 -> self#button_1_press x y shifted double
      | 3 -> self#context_menu x y button (GdkEvent.Button.time ev)
      | _ -> ());
    true


  (***************************************************************************
   *
   * Pointer motion events
   *
   ***************************************************************************)

  (** Action for a pointer motion event that is part of dragging.
      Moves the viewport to the drawing area relative to the mouse position
      when dragging started.
  *)
  method pointer_motion (_ev : GdkEvent.Motion.t) =
    let (x, y) = Gdk.Window.get_pointer_location top_window#misc#window in
    let new_h_value = 
      last_button_press_h_adjustment_value +.
    	!current_config.button_1_drag_acceleration *.
	(float_of_int (x - last_button_press_top_x))
    in
    let new_v_value = 
      last_button_press_v_adjustment_value +.
    	!current_config.button_1_drag_acceleration *. 
	(float_of_int (y - last_button_press_top_y))
    in
    (* 
     * let hint = GdkEvent.Motion.is_hint _ev in
     * Printf.fprintf (debugc()) 
     * "PM %d %d%s\n%!" x y (if hint then " H" else "");
     *)
    if not restored_selected_node 
    then self#single_restore_selected_node;
    drawing_h_adjustment#set_value 
      (min new_h_value 
    	 (drawing_h_adjustment#upper -. drawing_h_adjustment#page_size));
    drawing_v_adjustment#set_value 
      (min new_v_value
    	 (drawing_v_adjustment#upper -. drawing_v_adjustment#page_size));
    (* 
     * last_button_1_x <- x;
     * last_button_1_y <- y;
     *)
    true

  (** General callback for all pointer motion events.  *)
  method pointer_general_motion (ev : GdkEvent.Motion.t) =
    if Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Motion.state ev)
    then self#pointer_motion ev
    else false


  (***************************************************************************
   *
   * menu actions
   *
   ***************************************************************************)

  (** Field to store the undo state number. This number is determined
      when the context menu is posted and is needed in the callback for
      the undo menu entry. *)
  val mutable context_menu_undo_state = None

  (** Field to store the proof command under the mouse when the
      context menu is posted. This node is needed for sending proof
      commands and proof scripts. To avoid a memory leak, this field
      is cleared with an idle action, see {!context_menu_deactivated}.
  *)
  val mutable context_menu_node = None

  (** Callback for the undo menu entry. *)
  method undo_to_point () = 
    match context_menu_undo_state with
      | None -> assert false
      | Some state -> emacs_callback_undo state

  (** Callback for inserting one proof command. *)
  method insert_proof_command () = 
    match context_menu_node with
      | None -> assert false
      | Some node -> emacs_send_proof_script node#content

  (** Callback for inserting all proof commands of a subtree. *)
  method insert_subproof () =
    let buf = Buffer.create 4095 in
    let rec collect node indent =
      Buffer.add_string buf (String.make indent ' ');
      Buffer.add_string buf node#content;
      Buffer.add_char buf '\n';
      let sequents = node#children in
      ignore(
	List.fold_left
	  (fun indent sequent ->
	    match sequent#children with
	      | [pc] -> collect pc indent; indent - 2
	      | [] -> indent - 2        (* happens for the current sequent *)
	      | _ -> assert false)
	  (indent + (2 * List.length sequents) - 2)
	  sequents)
    in
    match context_menu_node with
      | None -> assert false
      | Some node -> 
	collect node 0;
	emacs_send_proof_script (Buffer.contents buf)

  (** Post the context menu. Depending on where the mouse button is
      pressed, certain menu entries as disabled. The undo state is
      stored in {!context_menu_undo_state} for use in the callback for
      the undo entry.
  *)
  method context_menu x y button time =
    self#locate_button_node x y
      (fun node -> 
	context_menu_node <- Some node;
	let undo_state = 
	  if disconnected then None
	  else
	    match node#node_kind with
	      | Proof_command -> None
	      | Turnstile -> match node#children with
		  | first :: _ -> Some first#undo_state
		  | _ -> None
	in
	context_menu_undo_state <- undo_state;
	(List.nth context_menu#children 0)#misc#set_sensitive
	  (undo_state <> None);
	(List.nth context_menu#children 1)#misc#set_sensitive
	  (node#node_kind = Proof_command);
	(List.nth context_menu#children 2)#misc#set_sensitive
	  (node#node_kind = Proof_command);
	context_menu#popup ~button ~time;
      )
      (fun () ->
	List.iter
	  (fun n ->
	    (List.nth context_menu#children n)#misc#set_sensitive false)
	  [0; 1; 2];
	context_menu#popup ~button ~time
      )

  (** Callback for the deactivated signal of the context menu.
      Unfortunately, this signal is processed before the callback for
      the selected menu item is processed. Therefore, the
      {!context_menu_node} cannot be cleared directly but only in an
      idle action.
  *)
  method context_menu_deactivated () =
    ignore(GMain.Idle.add (fun () -> context_menu_node <- None; false))


  (***************************************************************************
   *
   * tooltips
   *
   ***************************************************************************)
      
  (** Callback for the query tooltip event. Display a tool-tip if
      there is a sequent node or an abbreviated proof command node.
  *)
  method drawable_tooltip ~x ~y ~kbd:(_kbd : bool) (tooltip : Gtk.tooltip) =
    (* Printf.fprintf (debugc()) "TTS x %d y %d\n%!" x y; *)
    self#locate_button_node x y 
      (fun node -> match node#node_kind with
	| Turnstile -> 
	  if !current_config.display_turnstile_tooltips then begin
	    let contents = GMisc.label ~text:node#displayed_text () in
	    GtkBase.Tooltip.set_custom tooltip contents#as_widget;
	    true
	  end 
	  else false
	| Proof_command ->
	  if !current_config.display_command_tooltips && node#content_shortened
	  then begin
	    let contents = GMisc.label ~text:node#displayed_text () in
	    GtkBase.Tooltip.set_custom tooltip contents#as_widget;
	    true
	  end
	  else false
      )
      (fun () -> false)


  (***************************************************************************
   *
   * Cloning
   *
   ***************************************************************************)

  (** Callback for the clone menu item. Creates and displays a clone
      window.
  *)
  method clone (owin : proof_window) =
    owin#set_clone_flag;
    let old_selected = match current_node with
      | Some _ -> current_node
      | None -> selected_node
    in
    let cloned_selected = ref None in
    let cloned_layers = 
      layer_stack#clone_layers (owin#new_proof_command 0) (owin#new_turnstile 0)
        old_selected cloned_selected in
    owin#layer_stack#set_layers cloned_layers;
    owin#set_selected_node !cloned_selected;
    owin#refresh_and_position;
    cloned_proof_windows := owin :: !cloned_proof_windows


  (***************************************************************************
   *
   * Proof element creation
   *
   ***************************************************************************)

  (** Create a new turnstile node for sequents. *)
  method new_turnstile undo_state sequent_id sequent_text_option =
    new turnstile drawable undo_state sequent_id sequent_text_option

  (** Create a new proof command node. *)
  method new_proof_command undo_state command 
                           inst_existentials new_existentials =
    new proof_command drawable undo_state command command 
      (inst_existentials : existential_variable list)
      (new_existentials : existential_variable list)
end



(*****************************************************************************
 *
 * proof window creation
 *
 *****************************************************************************)

(** Create a new proof tree window. Creates the widget hierarchy, the
    menu, initializes the management object and registers all
    callbacks.
*)
let rec make_proof_window name geometry_string =
  let top_window = GWindow.window () in
  top_window#set_default_size 
    ~width:!current_config.default_width_proof_tree_window
    ~height:!current_config.default_height_proof_tree_window;
  (* top_v_box for the pane and the button hbox *)
  let top_v_box = GPack.vbox ~packing:top_window#add () in
      (* top_paned for the drawing area and the sequent *)
  let top_paned = GPack.paned `VERTICAL 
    ~packing:(top_v_box#pack ~expand:true) ()
  in
  let drawing_scrolling = GBin.scrolled_window (* ~border_width:1 *)
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
    ~packing:(top_paned#pack1 ~resize:true ~shrink:false) () 
  in
  let drawing_h_adjustment = drawing_scrolling#hadjustment in
  let drawing_v_adjustment = drawing_scrolling#vadjustment in
  let drawing_area = GMisc.drawing_area 
    ~packing:drawing_scrolling#add_with_viewport () 
  in
  let _ = drawing_area#misc#realize () in
  let drawable = 
    new better_drawable drawing_area#misc#window 
      drawing_area#misc#create_pango_context
  in
  let outer_sequent_frame = GBin.frame ~shadow_type:`IN 
    ~packing:(top_paned#pack2 ~resize:false ~shrink:true) () 
  in
  let labeled_sequent_frame = GBin.frame ~label:"no sequent" ~shadow_type:`NONE
    ~packing:outer_sequent_frame#add ()
  in
  let sequent_scrolling = GBin.scrolled_window 
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
    ~packing:labeled_sequent_frame#add () 
  in
  (* 
   * let sequent_h_adjustment = sequent_scrolling#hadjustment in
   *)
  let sequent_v_adjustment = sequent_scrolling#vadjustment in
  sequent_scrolling#misc#modify_font !sequent_font_desc;
  let context = sequent_scrolling#misc#pango_context in
  let layout = context#create_layout in
  Pango.Layout.set_text layout "X";
  let (_, char_height) = Pango.Layout.get_pixel_size layout in
  let sequent_window = GText.view ~editable:false ~cursor_visible:false
    ~height:(char_height * !current_config.internal_sequent_window_lines)
    ~packing:sequent_scrolling#add () 
  in
  sequent_window#misc#modify_font !sequent_font_desc;

  (* bottom button line with the message in the middle *)
  let button_box_align = GBin.alignment
    ~padding:(1,1,3,3)			(* (top, bottom, left, right)*)
    ~packing:(top_v_box#pack) () in
  let button_h_box = GPack.hbox
    ~packing:button_box_align#add () in
  let dismiss_button = 
    GButton.button ~stock:`CLOSE ~packing:button_h_box#pack ()
  in
  let message_label =
    GMisc.label ~selectable:true ~ellipsize:`END 
      ~packing:(button_h_box#pack ~expand:true ~fill:true) ()
  in
  message_label#set_use_markup true;
  let menu_button = 
    GButton.button ~label:"Menu" ~packing:(button_h_box#pack) ()
  in

  let main_menu = GMenu.menu () in

  let context_menu = GMenu.menu () in

  let proof_window = 
    new proof_window top_window 
      drawing_h_adjustment drawing_v_adjustment drawing_area
      drawable labeled_sequent_frame sequent_window sequent_v_adjustment
      message_label context_menu name
  in
  let clone_fun () =
    let owin = make_proof_window name geometry_string in
    proof_window#clone owin
  in

  let main_menu_entries = 
    [`I("Clone", clone_fun);
     `I("Show current", proof_window#reposition_current_node);
     `I("Show selected", proof_window#show_selected_node);
     `I("Existentials", proof_window#show_existential_window);
     `I("Configuration", show_config_window);
     `I("Help", show_help_window);
     `I("About", show_about_window);
     `I("Exit", (fun _ -> exit 0));
    ] in
  let context_menu_entries =
    [`I("Undo to point", proof_window#undo_to_point);
     `I("Insert command", proof_window#insert_proof_command);
     `I("Insert subproof", proof_window#insert_subproof);
     `S
    ] @ main_menu_entries
  in
  GToolbox.build_menu main_menu ~entries:main_menu_entries;
  GToolbox.build_menu context_menu ~entries:context_menu_entries;
  ignore(context_menu#connect#deactivate 
           ~callback:(proof_window#context_menu_deactivated));

  top_window#set_title (name ^ " proof tree");
  drawable#set_line_attributes 
    ~width:(!current_config.turnstile_line_width) ();
  drawing_area#misc#set_has_tooltip true;
  ignore(drawing_area#misc#connect#query_tooltip
   	   ~callback:proof_window#drawable_tooltip);
  ignore(drawing_scrolling#misc#connect#size_allocate
	   ~callback:proof_window#draw_scroll_size_allocate_callback);
  (* 
   * ignore(drawing_area#misc#connect#size_allocate
   * 	   ~callback:proof_window#draw_area_size_allocate_callback);
   *)
  (* 
   * ignore(drawing_area#event#connect#configure
   * 	   ~callback:proof_window#draw_area_configure_callback);
   *)

  ignore(top_window#connect#destroy 
	   ~callback:proof_window#user_delete_proof_window);

  (* the delete event yields a destroy signal if not handled *)
  (* ignore(top_window#event#connect#delete 
     ~callback:proof_window#delete_proof_window); *)

    (* 
     * ignore(drawing_area#misc#set_can_focus true);
     * ignore(drawing_area#event#connect#key_press 
     *                  proof_window#key_pressed_callback);
     *)
  ignore(top_window#event#connect#key_press 
	   ~callback:proof_window#key_pressed_callback);
  ignore(drawing_area#event#connect#expose 
	   ~callback:proof_window#expose_callback);
  (* ignore(drawing_area#misc#connect#size_allocate ~callback:resize); *)

  (* events to receive: 
   *  - all button presses, 
   *  - pointer motion when button 1 is pressed
   *  - reduced number of pointer motion events
   *)
  ignore(drawing_area#event#add 
	   [`BUTTON_PRESS; `BUTTON1_MOTION; `POINTER_MOTION_HINT]);
  ignore(drawing_area#event#connect#button_press 
	   ~callback:proof_window#button_press);
  ignore(drawing_area#event#connect#motion_notify
	   ~callback:proof_window#pointer_general_motion);

  ignore(sequent_v_adjustment#connect#changed 
	   ~callback:proof_window#sequent_area_changed);

  ignore(dismiss_button#connect#clicked 
	   ~callback:proof_window#user_delete_proof_window);
  ignore(menu_button#connect#clicked 
	   ~callback:(fun () -> 
	     main_menu#popup ~button:0 
	       ~time:(GtkMain.Main.get_current_event_time ())));

  top_window#show ();
  if geometry_string <> "" then
    ignore(top_window#parse_geometry geometry_string);
  if !current_config.internal_sequent_window_lines = 0 then
    top_paned#set_position (top_paned#max_position);

  proof_window
