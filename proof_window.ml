open Util
open Gtk_ext
open Draw_tree

class proof_window top_window 
  drawing_h_adjustment drawing_v_adjustment (drawing_area : GMisc.drawing_area)
  drawable_arg 
  =
  let start_node = (new turnstile drawable_arg "top" :> proof_tree_element) in
object (self)
  val top_window = top_window
  val drawing_h_adjustment = drawing_h_adjustment
  val drawing_v_adjustment = drawing_v_adjustment
  val drawing_area = drawing_area
  val drawable = drawable_arg

  val mutable current_leafs = [start_node]

  val mutable top_left = 0

  val mutable tree = start_node

  method scroll (adjustment : GData.adjustment) direction =
    let a = adjustment in
    let new_val = a#value +. float_of_int(direction) *. a#step_increment in
    let new_val = if new_val < 0.0 then 0.0 else new_val in
    let max = max 0.0 (a#upper -. a#page_size) in
    let new_val = if new_val > max then max else new_val in
    a#set_value new_val

  method delete_proof_window () =
    GMain.quit()

  method delete_proof_window_event _ =
    self#delete_proof_window ();
    true

  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
      | ks when (ks = GdkKeysyms._Q or ks = GdkKeysyms._q)  -> 
	self#delete_proof_window_event ev
      | ks when (ks = GdkKeysyms._E or ks = GdkKeysyms._e)  -> 
	self#extend_leaf; true
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

  method proof_tree_changed =
    let (current_width, current_height) = drawable#size in
    let new_width = tree#subtree_width in
    let new_height = tree#subtree_height in
    (* 
     * Printf.eprintf "RS %d x %d -> %d x %d\n%!"
     *   current_width current_height new_width new_height;
     *)
    if new_width > current_width || new_height > current_height then
      drawing_area#misc#set_size_request
	~width:(max current_width new_width)
	~height:(max current_height new_height) ();
    let (new_width, _) = drawable#size in
    top_left <- max 0 ((new_width - tree#subtree_width) / 2);
    self#redraw

  val mutable extend_counter = 1

  method extend_leaf =
    let leaf = List.hd current_leafs in
    let e1 = 
      new proof_command drawable_arg
	(* "abcdefghijklmnopqrstuvwxyz0123456789" *)
	"destruct bla blup"
	(Printf.sprintf "com %d" extend_counter )
    in
    let t1 = (new turnstile drawable_arg 
		(Printf.sprintf "turn %d" (extend_counter + 1))
	      :> proof_tree_element) 
    in
    let t2 = (new turnstile drawable_arg
		(Printf.sprintf "turn %d" (extend_counter + 2))
	      :> proof_tree_element)
    in
    let t3 = (new turnstile drawable_arg
		(Printf.sprintf "turn %d" (extend_counter + 3))
	      :> proof_tree_element)
    in
    extend_counter <- extend_counter + 4;
    set_children leaf [e1];
    set_children e1 [t1; t2; t3];
    current_leafs <- (List.tl current_leafs) @ [t1; t2; t3];
    self#proof_tree_changed

  method redraw =
    self#erase;
    (* let left = 0 in *)
    ignore(tree#draw_subtree top_left 0)

  method expose_callback (ev : GdkEvent.Expose.t) =
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
    tree#mouse_button_tree top_left 0 x y button;
    true
end

let make_proof_window geometry_string =
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
  let sequent_frame_1 = GBin.frame (* ~label:"Sequent" ~label_xalign:1.0  *)
    (* ~border_width:1 *) ~shadow_type:`IN 
    ~packing:(top_paned#pack2 ~resize:false ~shrink:false) () 
  in
  let sequent_frame_2 = GBin.frame ~label:"Sequent" (* ~label_xalign:1.0 *)
    (* ~border_width:0 *) ~shadow_type:`NONE
    ~packing:sequent_frame_1#add ()
  in
  let sequent_scrolling = GBin.scrolled_window 
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
    ~packing:sequent_frame_2#add () 
  in
  (* 
   * let sequent_v_adjustment = drawing_scrolling#vadjustment in
   * let sequent_h_adjustment = drawing_scrolling#hadjustment in
   *)
  let _sequent_window = GText.view ~editable:false ~cursor_visible:false
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
      drawable
  in
  ignore(top_window#connect#destroy ~callback:proof_window#delete_proof_window);
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
	   ~callback:proof_window#delete_proof_window);

  top_window#show ();
  if geometry_string <> "" then
    ignore(top_window#parse_geometry geometry_string);
  proof_window#proof_tree_changed;

  proof_window
