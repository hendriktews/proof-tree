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
 * $Id: proof_window.ml,v 1.38 2011/12/08 15:47:12 tews Exp $
 *)


(** Creation, display and drawing of the proof tree window *)


open Util
open Configuration
open Gtk_ext
open Draw_tree
open Node_window
open Help_window
open About_window
open Ext_dialog


(** Callback for higher-level modules when the user deletes a proof
    window. During start-up the reference is changed to
    {!Proof_tree.clear_proof_tree_lists}.
*)
let delete_proof_tree_callback = ref (fun (_ : string) -> ())


(** Contains proof window clones. See also
    [Proof_tree.all_proof_trees_for_undo] and
    [Proof_tree.undo_surviver_trees].
*)
let cloned_proof_windows = ref []

class proof_window (top_window : GWindow.window)
  drawing_h_adjustment drawing_v_adjustment (drawing_area : GMisc.drawing_area)
  (drawable : better_drawable)
  labeled_sequent_frame sequent_window sequent_v_adjustment
  (message_label : GMisc.label) menu proof_name
  =
object (self)

  (***************************************************************************
   *
   * Internal state and setters/accessors
   *
   ***************************************************************************)
  val mutable top_left = 0
  val top_top = 0

  val mutable sequent_window_scroll_to_bottom = false

  val mutable root = None

  val mutable current_node = None
  val mutable current_node_offset_cache = None
  val mutable position_to_current_node = true

  val mutable selected_node = None

  (** List of all external non-orphaned node windows that belong to nodes 
      in the current proof tree. This list if stored only for optimization.
  *)
  val mutable node_windows = []

  (** The management object for the dialog for existential
      variables, if present. 
  *)
  val mutable existential_window = None

  method set_root r = 
    root <- Some (r : proof_tree_element)

  method clear_root = root <- None

  (** Return the selected node or [None] if there is none. *)
  method get_selected_node = selected_node

  method delete_node_window win =
    node_windows <- List.filter (fun owin -> owin <> win) node_windows


  (***************************************************************************
   *
   * Messages
   *
   ***************************************************************************)

  method message text = message_label#set_label text

  (***************************************************************************
   *
   * Sequent window
   *
   ***************************************************************************)

  method sequent_area_changed () =
    if sequent_window_scroll_to_bottom then
      let a = sequent_v_adjustment in
      a#set_value (max a#lower (a#upper -. a#page_size))

  method private update_sequent_area label content scroll_to_bottom =
    labeled_sequent_frame#set_label (Some label);
    sequent_window#buffer#set_text content;
    sequent_window_scroll_to_bottom <- scroll_to_bottom

  method private clear_sequent_area =
    labeled_sequent_frame#set_label (Some "no sequent");
    sequent_window#buffer#set_text "";
    sequent_window_scroll_to_bottom <- false;

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

  method set_current_node n =
    current_node_offset_cache <- None;
    current_node <- Some (n : proof_tree_element)

  method clear_current_node =
    current_node_offset_cache <- None;
    current_node <- None;


  (***************************************************************************
   *
   * Unclassified methods
   *
   ***************************************************************************)

  method survive_undo_before_start =
    match root with
      | None -> false
      | Some root -> root#children <> []

  method disconnect_proof =
    match root with
      | None -> ()
      | Some root -> root#disconnect_proof

  method configuration_updated =
    let rec update_tree_sizes node =
      List.iter update_tree_sizes node#children;
      node#configuration_updated
    in
    sequent_window#misc#modify_font !sequent_font_desc;
    drawable#set_line_attributes 
      ~width:(!current_config.turnstile_line_width) ();
    (match root with
      | None -> ()
      | Some root -> update_tree_sizes root
    );
    self#expand_drawing_area;
    ignore(self#position_tree);
    GtkBase.Widget.queue_draw top_window#as_widget
    
  method clear_for_reuse =
    root <- None;
    current_node <- None;
    current_node_offset_cache <- None;
    selected_node <- None;
    self#refresh_sequent_area;
    List.iter (fun w -> w#delete_non_sticky_node_window) node_windows;
    assert(node_windows = []);
    self#clear_existential_dialog_for_reuse

  method update_existentials_display =
    match root with
      | None -> ()
      | Some root -> root#update_existentials_display

  method find_node p =
    let res = ref None in
    let rec iter node =
      if p node 
      then begin
	res := Some node;
	raise Exit
      end
      else List.iter iter node#children
    in
    (match root with
      | None -> ()
      | Some root ->
	try iter root
	with Exit -> ()
    );
    !res


  (***************************************************************************
   *
   * Position to nodes and points
   *
   ***************************************************************************)

  method private clamp_to_node node =
    let (x_l, x_u, y_l, y_u) = node#bounding_box top_left top_top in
    drawing_h_adjustment#clamp_page ~lower:x_l ~upper:x_u;
    drawing_v_adjustment#clamp_page ~lower:y_l ~upper:y_u


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

  method private centre_node node =
    let (x_l, x_u, y_l, y_u) = node#bounding_box top_left top_top in
    self#centre_point ((x_l +. x_u) /. 2.0) ((y_l +. y_u) /. 2.0)

  method private is_partially_visible node =
    let (node_x_l, node_x_u, node_y_l, node_y_u) = 
      node#bounding_box top_left top_top in
    let in_x = inside_adj_range drawing_h_adjustment in
    let in_y = inside_adj_range drawing_v_adjustment
    in
    ((in_x node_x_l) || (in_x node_x_u))
    && ((in_y node_y_l) || (in_y node_y_u))

  method show_node node =
    if self#is_partially_visible node 
    then self#clamp_to_node node
    else self#centre_node node

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

  method private scroll (adjustment : GData.adjustment) direction =
    let a = adjustment in
    let new_val = a#value +. float_of_int(direction) *. a#step_increment in
    let new_val = if new_val < 0.0 then 0.0 else new_val in
    let max = max 0.0 (a#upper -. a#page_size) in
    let new_val = if new_val > max then max else new_val in
    a#set_value new_val

  method delete_proof_window =
    List.iter (fun w -> w#delete_non_sticky_node_window) node_windows;
    self#destroy_existential_dialog;
    let self = (self :> proof_window) in
    cloned_proof_windows :=
      List.fold_left 
        (fun res w -> if w = self then res else w :: res)
        [] !cloned_proof_windows;
    top_window#destroy()

  method user_delete_proof_window () =
    !delete_proof_tree_callback proof_name;
    self#delete_proof_window

  method private delete_proof_window_event _ =
    self#user_delete_proof_window ();
    true

  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
      | ks when 
	  (ks = GdkKeysyms._Q or ks = GdkKeysyms._q) 
	  && (List.mem `CONTROL (GdkEvent.Key.state ev))
	  -> 
	exit 0
      | ks when (ks = GdkKeysyms._Q or ks = GdkKeysyms._q)  -> 
	self#delete_proof_window_event ev
      | ks when ks = GdkKeysyms._Left -> 
	self#scroll drawing_h_adjustment (-1); true
      | ks when ks = GdkKeysyms._Right -> 
	self#scroll drawing_h_adjustment 1; true
      | ks when ks = GdkKeysyms._Up -> 
	self#scroll drawing_v_adjustment (-1); true
      | ks when ks = GdkKeysyms._Down -> 
	self#scroll drawing_v_adjustment 1; true

      (* 
       * | ks when (ks = GdkKeysyms._E or ks = GdkKeysyms._e)  -> 
       * 	self#show_existential_window (); true
       *)

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

  method existential_clear () = 
    existential_window <- None

  (** Show a dialog for existential variables. If there is currently
      none, a new one is created.
  *)
  method show_existential_window () =
    match existential_window with
      | Some w -> w#present
      | None -> 
	let ext = 
	  make_ext_dialog (self :> proof_window) proof_name 
	in
	(match root with
	  | None -> ()
	  | Some root_node -> ext#fill_table root_node
	);
	existential_window <- Some ext

  method destroy_existential_dialog =
    match existential_window with
      | None -> ()
      | Some w -> 
	w#destroy ();
	existential_window <- None

  method clear_existential_dialog_for_reuse =
    match existential_window with
      | None -> ()
      | Some w -> w#clear_for_reuse

  method ext_dialog_add status_ext new_ext =
    match existential_window with
      | Some w -> w#change_and_add status_ext new_ext
      | None -> ()

  method ext_dialog_undo status_ext remove_ext =
    match existential_window with
      | Some w -> w#change_and_delete status_ext remove_ext
      | None -> ()

  method update_ext_dialog =
    match existential_window with
      | Some w -> w#update_ext_dialog
      | None -> ()


  (***************************************************************************
   *
   * Redraw / expose events
   *
   ***************************************************************************)

  method private get_current_offset =
    match current_node_offset_cache with
      | Some _ as res -> res
      | None -> match current_node with
	  | None -> None
	  | Some node ->
	    let res = Some node#bounding_box_offsets in
	    current_node_offset_cache <- res;
	    res

  method private erase = 
    (* Printf.fprintf (debugc()) "ERASE\n%!"; *)
    let (x,y) = drawable#size in
    let fg = drawable#get_foreground in
    drawable#set_foreground (`NAME("white"));
    drawable#polygon ~filled:true [(0,0); (x,0); (x,y); (0,y)];
    drawable#set_foreground (`COLOR fg)

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
	    let (req_width, req_height) = match root with
	      | None -> (0,0)
	      | Some root -> (root#subtree_width, root#subtree_height)
	    in
	    if (float_of_int req_width) > drawing_h_adjustment#upper ||
	       (float_of_int req_height) > drawing_v_adjustment#upper
	    then begin
	      (* The resize request for the drawing are has not 
	       * been processed. It might happen that this resize
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

  method private expand_drawing_area =
    match root with
      | None -> ()
      | Some root -> 
	let new_width = root#subtree_width in
	let new_height = root#subtree_height in
	(* 
         * Printf.fprintf (debugc()) "DRAWING AREA SIZE REQUEST %d x %d\n%!" 
	 *   new_width new_height;
         *)
	(* 
         * if new_width > current_width || new_height > current_height then
	 *   drawing_area#misc#set_size_request
	 *     ~width:(max current_width new_width)
	 *     ~height:(max current_height new_height) ();
         *)
	drawing_area#misc#set_size_request
	  ~width:new_width ~height:new_height ();

  (** Sets the position of the proof tree in the drawing area by 
      computing [top_left]. Returns true if the position changed. 
      In that case the complete drawing area must be redrawn.
  *)
  method private position_tree =
    match root with
      | None -> false
      | Some root -> 
	let old_top_left = top_left in
	let (width, _) = drawable#size in
	top_left <- max 0 ((width - root#subtree_width) / 2);
	top_left <> old_top_left

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
    (* let left = 0 in *)
    match root with
      | None -> ()
      | Some root ->
	(* Printf.fprintf (debugc()) "REDRAW\n%!"; *)
	ignore(root#draw_tree_root top_left top_top)

  method invalidate_drawing_area =
    (* Printf.fprintf (debugc()) "INVALIDATE\n%!"; *)
    GtkBase.Widget.queue_draw drawing_area#as_widget

  (** Method for updating the display after the proof tree has changed.
      Adjusts the tree position in the drawing area, schedules a 
      complete redraw and makes the current node (if any) visible.
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

  (** Make the current node visible.
  *)
  method reposition_current_node () =
    position_to_current_node <- true;
    self#try_adjustment

  method show_current_node () =
    match current_node with
      | None -> ()
      | Some current ->
	if self#is_partially_visible current
	then self#show_node current
	else self#reposition_current_node ()

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

  val mutable last_node_number = 0

  method private next_node_number =
    last_node_number <- last_node_number + 1;
    last_node_number


  (***************************************************************************
   *
   * Button events
   *
   ***************************************************************************)

  val mutable last_button_press_top_x = 0
  val mutable last_button_press_top_y = 0
  val mutable last_button_press_v_adjustment_value = 0.0
  val mutable last_button_press_h_adjustment_value = 0.0

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


  val mutable old_old_selected_node = None
  val mutable old_selected_node = None
  val mutable restored_selected_node = false

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

  method private save_selected_node_state =
    old_old_selected_node <- old_selected_node;
    old_selected_node <- selected_node;
    restored_selected_node <- false

  method private single_restore_selected_node =
    self#set_selected_node old_selected_node;
    restored_selected_node <- true

  method private double_restore_selected_node =
    self#set_selected_node old_old_selected_node;
    restored_selected_node <- true

  method private locate_button_node : 
    'a . int -> int -> (#proof_tree_element -> 'a) -> (unit -> 'a) -> 'a =
    fun x y node_click_fun outside_click_fun ->
      let node_opt = match root with 
	| None -> None
	| Some root ->
	  root#mouse_button_tree_root top_left top_top x y
      in
      match node_opt with
	| None -> outside_click_fun ()
	| Some node -> node_click_fun node

  method private external_node_window (node : proof_tree_element) =
    let n = string_of_int(self#next_node_number) in
    let win = 
      make_node_window (self :> proof_window) proof_name node n 
    in 
    node_windows <- win :: node_windows;
    self#invalidate_drawing_area

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
     * Printf.fprintf (debugc()) "%s Button %s%d at %d x %d\n%!" 
     *   (match GdkEvent.get_type ev with
     * 	| `BUTTON_PRESS -> "single"
     * 	| `TWO_BUTTON_PRESS -> "double"
     * 	| `THREE_BUTTON_PRESS -> "triple"
     * 	| `BUTTON_RELEASE -> "release")
     *   (if shifted then "shift " else "")
     *   button x y;
     *)
    (match button with
      | 1 -> self#button_1_press x y shifted double
      | 3 -> menu#popup ~button ~time:(GdkEvent.Button.time ev)
      | _ -> ());
    true


  (***************************************************************************
   *
   * Pointer motion events
   *
   ***************************************************************************)

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

  method pointer_general_motion (ev : GdkEvent.Motion.t) =
    if Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Motion.state ev)
    then self#pointer_motion ev
    else false


  (***************************************************************************
   *
   * tooltips
   *
   ***************************************************************************)
      
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

  method clone (owin : proof_window) =
    let become_selected = match current_node with
      | Some _ -> current_node
      | None -> selected_node
    in
    let cloned_selected = ref None in
    let ex_hash = Hashtbl.create 251 in
    let rec copy_existential ex =
      try Hashtbl.find ex_hash ex.existential_name
      with
	| Not_found -> 
	  let deps = List.map copy_existential ex.dependencies in
	  let nex = { existential_name = ex.existential_name;
		      instantiated = ex.instantiated;
		      existential_mark = false;
		      dependencies = deps;
		    }
	  in
	  Hashtbl.add ex_hash ex.existential_name nex;
	  nex
    in
    let rec clone_tree node =
      let cloned_children = List.map clone_tree node#children in
      let clone = match node#node_kind with
	| Proof_command -> 
	  (owin#new_proof_command node#content 
	     (List.map copy_existential node#inst_existentials)
	     (List.map copy_existential node#fresh_existentials)
	   :> proof_tree_element)
	| Turnstile -> 
	  (owin#new_turnstile node#id node#content :> proof_tree_element)
      in
      if Some node = become_selected
      then cloned_selected := Some clone;
      set_children clone cloned_children;
      (match node#branch_state with
	| Cheated
	| Proven -> clone#set_branch_state node#branch_state
	| Unproven
	| CurrentNode
	| Current -> ()
      );
      clone
    in    
    (match root with
      | None -> ()
      | Some root_node ->
	let cloned_tree = clone_tree root_node in
	cloned_tree#propagate_existentials;
	owin#set_root cloned_tree
    );
    owin#set_selected_node !cloned_selected;
    owin#refresh_and_position;
    cloned_proof_windows := owin :: !cloned_proof_windows


  (***************************************************************************
   *
   * Proof element creation
   *
   ***************************************************************************)

  method new_turnstile sequent_id sequent_text =
    new turnstile drawable sequent_id sequent_text

  method new_proof_command command inst_existentials new_existentials =
    new proof_command drawable command command 
      (inst_existentials : existential_variable list)
      (new_existentials : existential_variable list)
end



(*****************************************************************************
 *
 * proof window creation
 *
 *****************************************************************************)

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
    GButton.button ~label:"Dismiss" ~packing:button_h_box#pack ()
  in
  let message_label =
    GMisc.label ~selectable:true ~ellipsize:`END 
      ~packing:(button_h_box#pack ~expand:true ~fill:true) ()
  in
  message_label#set_use_markup true;
  let menu_button = 
    GButton.button ~label:"Menu" ~packing:(button_h_box#pack) ()
  in

  let menu = GMenu.menu () in

  let proof_window = 
    new proof_window top_window 
      drawing_h_adjustment drawing_v_adjustment drawing_area
      drawable labeled_sequent_frame sequent_window sequent_v_adjustment
      message_label menu name
  in
  let clone_fun () =
    let owin = make_proof_window name geometry_string in
    proof_window#clone owin
  in
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
	     menu#popup ~button:0 
	       ~time:(GtkMain.Main.get_current_event_time ())));

  GToolbox.build_menu menu
    ~entries:[`I("Clone", clone_fun);
	      `I("Show current", proof_window#reposition_current_node);
	      `I("Existentials", proof_window#show_existential_window);
	      `I("Configuration", show_config_window);
	      `I("Help", show_help_window);
	      `I("About", show_about_window);
	      `I("Exit", (fun _ -> exit 0));
	     ];

  top_window#show ();
  if geometry_string <> "" then
    ignore(top_window#parse_geometry geometry_string);
  if !current_config.internal_sequent_window_lines = 0 then
    top_paned#set_position (top_paned#max_position);

  proof_window
