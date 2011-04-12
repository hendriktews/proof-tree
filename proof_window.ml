open Configuration
open Gtk_ext
open Draw_tree

let delete_proof_tree_callback = ref (fun (_ : string) -> ())

class proof_window top_window 
  drawing_h_adjustment drawing_v_adjustment (drawing_area : GMisc.drawing_area)
  drawable_arg labeled_sequent_frame sequent_window proof_name
  =
object (self)
  val top_window = (top_window : GWindow.window)
  val drawing_h_adjustment = drawing_h_adjustment
  val drawing_v_adjustment = drawing_v_adjustment
  val drawing_area = drawing_area
  val drawable : better_drawable = drawable_arg
  val labeled_sequent_frame = labeled_sequent_frame
  val sequent_window = sequent_window
  val proof_name = proof_name

  val mutable top_left = 0
  val top_top = 0

  val mutable root = None

  val mutable current_node = None
  val mutable current_node_offset_cache = None
  val mutable position_to_current_node = true

  val mutable selected_node = None

  method set_root r = 
    root <- Some (r : proof_tree_element)

  method clear_root = root <- None

  method disconnect_proof =
    match root with
      | None -> ()
      | Some root -> root#disconnect_proof

  method update_sequent label content =
    labeled_sequent_frame#set_label (Some label);
    sequent_window#buffer#set_text content;

  method set_current_node n =
    current_node_offset_cache <- None;
    current_node <- Some (n : proof_tree_element);
    if selected_node = None && n#node_kind = Turnstile then
      match n#parent with
	| None -> ()
	| Some p -> match p#parent with
	    | None -> ()
	    | Some p ->
	      self#update_sequent "Previous sequent" p#content

  method get_current_offset =
    match current_node_offset_cache with
      | Some _ as res -> res
      | None -> match current_node with
	  | None -> None
	  | Some node ->
	    let width_2 = node#width / 2 in
	    let height_2 = node#height / 2 in
	    let (x_off, y_off) = node#x_y_offsets in
	    let res = Some((x_off, y_off, width_2, height_2)) in
	    current_node_offset_cache <- res;
	    res

  method scroll (adjustment : GData.adjustment) direction =
    let a = adjustment in
    let new_val = a#value +. float_of_int(direction) *. a#step_increment in
    let new_val = if new_val < 0.0 then 0.0 else new_val in
    let max = max 0.0 (a#upper -. a#page_size) in
    let new_val = if new_val > max then max else new_val in
    a#set_value new_val

  method delete_proof_window =
    top_window#destroy()

  method user_delete_proof_window () =
    !delete_proof_tree_callback proof_name;
    self#delete_proof_window

  method delete_proof_window_event _ =
    self#user_delete_proof_window ();
    true

  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
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

      | _ -> false

  method erase = 
    let (x,y) = drawable#size in
    let fg = drawable#get_foreground in
    drawable#set_foreground (`NAME("white"));
    drawable#polygon ~filled:true [(0,0); (x,0); (x,y); (0,y)];
    drawable#set_foreground (`COLOR fg)

  method try_adjustment = 
    if position_to_current_node = true then
      match self#get_current_offset with
	| None -> ()
	| Some((x_off, y_off, width_2, height_2)) ->
	  let x_l = float_of_int(top_left + x_off - width_2) in
	  let x_u = float_of_int(top_left + x_off + width_2) in
	  let y_l = float_of_int(top_top + y_off - height_2) in
	  let y_u = float_of_int(top_top + y_off + height_2) in
          (* The following two clamp_page calls might immediately trigger
	   * expose events, which will call try_adjustment again. To avoid
	   * entering this function a second time before leaving it, I
	   * temporarily switch position_to_current_node off.
	   *)
	  position_to_current_node <- false;
	  drawing_h_adjustment#clamp_page ~lower:x_l ~upper:x_u;
	  drawing_v_adjustment#clamp_page ~lower:y_l ~upper:y_u;
	  let x_val = drawing_h_adjustment#value in
	  let x_size = drawing_h_adjustment#page_size in
	  let y_val = drawing_v_adjustment#value in
	  let y_size = drawing_v_adjustment#page_size in
	  if x_l >= x_val && x_u <= x_val +. x_size &&
	    y_l >= y_val && y_u <= y_val +. y_size
	  then
	    () (* Do nothing: leave position_to_current_node disabled *)
	  else
	    (* Schedule the adjustment again, hope that we are more
	     * successful next time.
	     *)
	    position_to_current_node <- true;
          (* 
	   * (let a = drawing_v_adjustment in
	   *  Printf.eprintf 
	   * 	 "TA %s VADJ low %f val %f up %f size %f step %f page %f\n%!"
	   * 	 (match scheduled_adjustment with | None -> "N" | Some _ -> "S")
	   * 	 a#lower a#value a#upper a#page_size 
	   * 	 a#step_increment a#page_increment)
	   *)

  method expand_drawing_area =
    match root with
      | None -> ()
      | Some root -> 
	let new_width = root#subtree_width in
	let new_height = root#subtree_height in
	(* 
         * if new_width > current_width || new_height > current_height then
	 *   drawing_area#misc#set_size_request
	 *     ~width:(max current_width new_width)
	 *     ~height:(max current_height new_height) ();
         *)
	drawing_area#misc#set_size_request
	  ~width:new_width ~height:new_height ();


  method position_tree =
    match root with
      | None -> ()
      | Some root -> 
	let (width, _) = drawable#size in
	top_left <- max 0 ((width - root#subtree_width) / 2);

  method redraw =
    (* 
     * (let a = drawing_v_adjustment in
     *  Printf.eprintf 
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
	ignore(root#draw_subtree top_left top_top)

  method invalidate_drawing_area =
    GtkBase.Widget.queue_draw drawing_area#as_widget

  method refresh_and_position =
    position_to_current_node <- true;
    self#expand_drawing_area;
    self#position_tree;
    self#try_adjustment;
    self#invalidate_drawing_area;

  method draw_scroll_size_allocate_callback (_size : Gtk.rectangle) =
    (* Printf.eprintf "SIZE ALLOC\n%!"; *)
    self#position_tree;
    (* 
     * (let a = drawing_v_adjustment in
     *  Printf.eprintf 
     *    "SA %s VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    (match scheduled_adjustment with | None -> "N" | Some _ -> "S")
     *    a#lower a#value a#upper a#page_size 
     *    a#step_increment a#page_increment);
     *)
    self#try_adjustment

  method expose_callback (ev : GdkEvent.Expose.t) =
    (* 
     * (let a = drawing_v_adjustment in
     *  Printf.eprintf "EX VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    a#lower a#value a#upper a#page_size 
     *    a#step_increment a#page_increment);
     *)
    (* 
     * (let a = drawing_h_adjustment in
     *  Printf.eprintf "HADJ low %f val %f up %f size %f step %f page %f\n"
     *    a#lower a#value a#upper a#page_size a#step_increment a#page_increment);
     * (let a = drawing_v_adjustment in
     *  Printf.eprintf "VADJ low %f val %f up %f size %f step %f page %f\n%!"
     *    a#lower a#value a#upper a#page_size a#step_increment a#page_increment);
     *)
    (* 
     * let r = GdkEvent.Expose.area ev in
     * Printf.eprintf "EXPOSE count %d %d x %d at %d x %d\n%!"
     *   (GdkEvent.Expose.count ev)
     *   (Gdk.Rectangle.width r) (Gdk.Rectangle.height r)
     *   (Gdk.Rectangle.x r) (Gdk.Rectangle.y r);
     *)
    self#redraw;
    (* prerr_endline "END EXPOSE EVENT"; *)
    false

  method locate_button_node x y f =
    let node_opt = match root with 
      | None -> None
      | Some root ->
	root#mouse_button_tree top_left top_top x y
    in
    match node_opt with
      | None -> ()
      | Some node -> f node

  method button_1_press node =
    (* Printf.eprintf "Click on %s\n%!" node#debug_name; *)
    (match selected_node with
      | None -> ()
      | Some onode -> onode#selected false);
    selected_node <- Some node;
    node#selected true;
    self#invalidate_drawing_area;
    let frame_text = match node#node_kind with
      | Turnstile -> "Selected sequent"
      | Proof_command -> "Selected command"
    in
    self#update_sequent frame_text node#content

  method button_press ev =
    let x = int_of_float(GdkEvent.Button.x ev +. 0.5) in
    let y = int_of_float(GdkEvent.Button.y ev +. 0.5) in
    let button = GdkEvent.Button.button ev in
    (* 
     * let state = B.state ev in
     * let mod_list = Gdk.Convert.modifier state in
     * let _ = Gdk.Convert.test_modifier `SHIFT state in
     *)
    (* Printf.printf "Button %d at %d x %d\n%!" button x y; *)
    (match button with
      | 1 -> self#locate_button_node x y self#button_1_press
      | _ -> ());
    true

  method new_turnstile sequent_id sequent_text =
    new turnstile drawable sequent_id sequent_text

  method new_proof_command command =
    new proof_command drawable command command
end

let make_proof_window name geometry_string =
  let top_window = GWindow.window () in
  top_window#set_default_size ~width:400 ~height:400;
  let top_v_box = GPack.vbox ~packing:top_window#add () in
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
    ~packing:(top_paned#pack2 ~resize:false ~shrink:false) () 
  in
  let labeled_sequent_frame = GBin.frame ~label:"Sequent" ~shadow_type:`NONE
    ~packing:outer_sequent_frame#add ()
  in
  let sequent_scrolling = GBin.scrolled_window 
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
    ~packing:labeled_sequent_frame#add () 
  in
  (* 
   * let sequent_v_adjustment = drawing_scrolling#vadjustment in
   * let sequent_h_adjustment = drawing_scrolling#hadjustment in
   *)
  let sequent_window = GText.view ~editable:false ~cursor_visible:false
    (* ~height:50 *)
    ~packing:sequent_scrolling#add () 
  in
  let button_h_box = GPack.hbox ~packing:top_v_box#pack () in
  let dismiss_button = 
    GButton.button ~label:"Dismiss" ~packing:button_h_box#pack ()
  in

  let proof_window = 
    new proof_window top_window 
      drawing_h_adjustment drawing_v_adjustment drawing_area
      drawable labeled_sequent_frame sequent_window name
  in
  drawable#set_line_attributes 
    ~width:(!current_config.turnstile_line_width) ();
  ignore(drawing_scrolling#misc#connect#size_allocate
	   ~callback:proof_window#draw_scroll_size_allocate_callback);
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
  ignore(top_window#event#connect#key_press proof_window#key_pressed_callback);
  ignore(drawing_area#event#connect#expose 
	   ~callback:proof_window#expose_callback);
  (* ignore(drawing_area#misc#connect#size_allocate ~callback:resize); *)
  ignore(drawing_area#event#add [`BUTTON_PRESS]);
  ignore(drawing_area#event#connect#button_press 
	   ~callback:proof_window#button_press);
  
  ignore(dismiss_button#connect#clicked 
	   ~callback:proof_window#user_delete_proof_window);

  top_window#show ();
  if geometry_string <> "" then
    ignore(top_window#parse_geometry geometry_string);

  proof_window
