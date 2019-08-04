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

(** The Existential Variable Dialog *)

(* open Util *)
open Gtk_ext
open Configuration
open Draw_tree


(** Record for one line in the table of existential variables. This
    record links the {!Draw_tree.existential_variable} record and the
    necessary GTK widgets in order to update the table line when the
    status of the existential changes. For memory management is makes
    also sense to destroy all widgets when a line gets deleted.

    XXX - seems wrong!
    For easy access, these table line records are stored in the
    {!existential_variable_window.ext_hash} hash table with the name
    of the existential as key.
*)
type ext_table_line = {
  ext_table_ext : existential_variable;     (** The existential record
						with all status information *)
  ext_table_row : int;                      (** The row number in the table *)
  ext_table_status_label : GMisc.label;     (** Label for the status *)
  ext_table_using_label : GMisc.label;      (** Label for using column *)
  ext_table_button : GButton.toggle_button; (** display toggle button *)
  ext_table_other : GObj.widget array;      (** all other widgets in the line *)
}


(** Class for managing dialogs for existential variables. Objects are
    created when the widget tree is completely constructed. Contains
    the necessary state and methods to handle all callbacks. The
    callbacks must be set up by the function that creates objects.

    The existential dialog supports lazy update as the proof-tree
    window. This means that the table is only updated when Prooftree
    is idle because there are currently no commands to process.
    Requests for adding or deleting existentials are accumulated in
    {!to_add} and {!to_delete} until {!update_ext_dialog} is called.
    Actually, the display must be updated add and delete requests
    (which is currently not ensured).

    Arguments are
    - proof_window      {!class: Proof_window.proof_window} to which this 
                        window is associated to
    - top_window	{xref lablgtk class GWindow.window}
                        of the top-level widget
    - ext_table		{xref lablgtk class GPack.table} of the main table
    - table_v_adjustment  {xref lablgtk class GData.adjustment} 
                          for the scrollbar of the table
    - border_vbars      the left and the right vertical bar together 
                        with their column
    - inner_vbars       the other vertical bars together with their column
    - start_row		first row in the table
    - id_col		column for internal evar name
    - name_col          column for external evar name
    - status_col	column for status
    - button_col	column for buttons
    - last_col		last column
*)
class existential_variable_window
  proof_window
  (top_window : GWindow.window)
  (ext_table : GPack.table)
  (table_v_adjustment : GData.adjustment)
  (border_vbars : (GObj.widget * int) array)
  (inner_vbars : (GObj.widget * int) array)
  start_row id_col name_col status_col using_col button_col last_col
  =
object (self)

  (** "no" label text for the instantiated column. *)
  val uninst_label = "open"

  (** "partially" label text for the instantiated column. Contains
      pango markup for the color.
  *)
  val mutable partially_inst_label = ""

  (** "fully" label text for the instantiated column. Contains pango
      markup for the color.
  *)
  val mutable fully_inst_label = ""

  (** Used to show the bottom of the existential variable table by
      default. If the user scrolls the table, then this flag remembers
      if he scrolled to the bottom, see
      {!scrollbar_value_changed_callback}. When the table size
      changes, the table is automatically scrolled to the bottom, if
      this flag is true.
  *)
  val mutable clamp_to_bottom = true

  (** The next free row in the table. Managed by all methods that add
      or delete rows in the table.
  *)
  val mutable next_row = start_row

  (** Hash mapping existential names to {!ext_table_line} records. The
      hash is used when updating the status and when deleting selected
      existentials from the table.
  *)
  val ext_hash = Hashtbl.create 251

  (** Existentials queued for addition. This is a list of lists to
      avoid list concatenation. If this is non-nil then {!to_delete}
      must be nil. *)
  val mutable to_add = []

  (** Existentials queued for deletion. This is a list of lists to
      avoid list concatenation. If this is non-nil then {!to_add} must
      be nil. *)
  val mutable to_delete = []

  (** This array is non-empty precisely when the "no existential
      variables" line is displayed in the array. Contains the widgets
      of that line if non-empty. 
  *)
  val mutable no_ext_label_widgets = [| |]

  (** Stores the currently pressed "mark" button if there is one. Used
      to ensure that maximal one mark button can be pressed.
  *)
  val mutable pressed_mark_button = None

  (** Create the pango markup text for the "partially" and "fully"
      labels. Called in the initializer and when the configuration has
      been updated.
  *)
  method make_inst_labels =
    partially_inst_label <- 
      pango_markup_color "partially" !proved_partial_gdk_color;
    fully_inst_label <- 
      pango_markup_color "fully" !proved_complete_gdk_color

  (** Schedule the existentials in the argument list to be added to
      the table.
  *)
  method change_and_add new_ext =
    assert(to_delete = []);
    if new_ext <> [] then
      to_add <- new_ext :: to_add

  (** Schedule the existentials in the argument list to be removed
      from the table.
  *)
  method change_and_delete remove_ext =
    assert(to_add = []);
    if remove_ext <> [] then
      to_delete <- remove_ext :: to_delete

  (** Adjust the length of the vertical bars in the table. The border
      bars are always drawn to the bottom of the table. The inner bars
      are only drawn up to row [bottom].
      
      All vertical bars are created in the very beginning and then
      reused all the time by removing and adding them to the table.
  *)
  method private adjust_vbars bottom =
    Array.iter 
      (fun (bar, col) ->
	ext_table#remove bar;
	ext_table#attach ~left:col ~top:0 ~bottom:next_row bar)
      border_vbars;
    Array.iter 
      (fun (bar, col) ->
	ext_table#remove bar;
	ext_table#attach ~left:col ~top:0 ~bottom bar)
      inner_vbars

  (** Put a "no existential variables" line into the table and store
      its widgets in {!no_ext_label_widgets}.
  *)
  method private make_no_ext_label =
    let no_ext_label = GMisc.label 
      ~text:"There are no existential variables" ~ypad:8
      ~packing:(ext_table#attach ~left:id_col ~right:last_col ~top:next_row)
      () in
    next_row <- next_row + 1;
    let hbar = GMisc.separator `HORIZONTAL 
      ~packing:(ext_table#attach ~left:0 ~right:last_col ~top:next_row) () in
    next_row <- next_row + 1;
    self#adjust_vbars (next_row - 2);
    no_ext_label_widgets <- [| hbar#coerce; no_ext_label#coerce |]

  (** Delete the "no existential variables" line from the table. *)
  method private delete_no_ext_label =
    Array.iter (fun w -> w#destroy ()) no_ext_label_widgets;
    no_ext_label_widgets <- [| |];
    next_row <- start_row

  (** Release the currently pressed "mark" button, if there is one. *)
  method release_pressed_button =
    (match pressed_mark_button with
      | None -> ()
      | Some button -> 
	button#set_active false;
	pressed_mark_button <- None
    );

  (** Return the pair of proof-tree nodes that create and instantiate
      the given existential. If the existential is not instantiated
      then the second component is [None].
  *)
  method private get_ext_nodes existential =
    let crea_node = 
      match 
	proof_window#find_node
	  (fun n -> List.memq existential n#fresh_existentials)
      with
	| None -> assert false
	| Some n -> n
    in
    (* 
     * Printf.fprintf (debugc())
     *   "GEN ext %s status %s crea node %s\n%!"
     *   existential.existential_name
     *   (match existential.status with
     * 	| Uninstantiated -> "uninst"
     * 	| Partially_instantiated -> "partial"
     * 	| Fully_instantiated -> "fully")
     *   crea_node#debug_name;
     *)
    if existential.evar_status <> Uninstantiated then
      let inst_node =  
	proof_window#find_node
	  (fun n -> List.memq existential n#inst_existentials)
      in
      assert(inst_node <> None);
      (crea_node, inst_node)
    else
      (crea_node, None)

  (** Callback function for toggling some "mark" button. Marks or
      unmarks the existential, keeps {!pressed_mark_button} up to date,
      schedules the proof tree for redisplay and tries to show the marked
      nodes.
  *)
  method mark_button_toggled button existential () =
    if button#active
    then begin
      self#release_pressed_button;
      pressed_mark_button <- Some button;
      existential.evar_mark <- true;
      (match self#get_ext_nodes existential with
	| (crea, Some inst) ->
	  proof_window#show_both_nodes crea inst
	| (crea, None) -> 
	  proof_window#show_node crea
      );
      proof_window#invalidate_drawing_area;
    end
    else begin
      existential.evar_mark <- false;
      pressed_mark_button <- None;
      proof_window#invalidate_drawing_area;
    end

  (** Update the information in the argument table line. *)
  method private set_ext_line_status tl =
    tl.ext_table_status_label#set_label
      (match tl.ext_table_ext.evar_status with
	| Uninstantiated -> uninst_label
	| Partially_instantiated -> partially_inst_label
	| Fully_instantiated -> fully_inst_label
      );
    let color_ex_name ex =
      match ex.evar_status with
	| Uninstantiated -> ex.evar_internal_name
	| Partially_instantiated -> 
	  pango_markup_color ex.evar_internal_name !proved_partial_gdk_color
	| Fully_instantiated -> 
	  pango_markup_color ex.evar_internal_name !proved_complete_gdk_color
    in
    let colored_dep_names =
      String.concat ", " 
	(List.map color_ex_name tl.ext_table_ext.evar_deps) in
    tl.ext_table_using_label#set_label colored_dep_names
      

  (** Update the status of the complete table. *)
  method private update_existential_status =
    Hashtbl.iter 
      (fun _ tl ->
	(* We don't have to update every line. However, the test is
	 * rather complicated, because we have to check whether the
	 * status of any of the dependencies changes. For that, one
	 * would have to go through ext_hash, in order to find the last
	 * status of the dependencies.
	 *)
	self#set_ext_line_status tl
      )
      ext_hash

  (** Add the existentials in the argument list to the table. Creates
      the necessary widgets and stores them in {!ext_hash}. This method
      must not be called with an empty argument list.
  *)
  method private process_fresh_existentials l =
    assert(l <> []);
    let doit ext =
      assert(Hashtbl.mem ext_hash ext.evar_internal_name = false);
      let ext_row = next_row in
      let id_label = GMisc.label ~text:ext.evar_internal_name
	~packing:(ext_table#attach ~left:id_col ~top:ext_row) () in
      let name_label_text = match ext.evar_external_name with
          | Some name -> name
          | None -> "—"
      in
      let name_label = GMisc.label ~text:name_label_text
        ~packing:(ext_table#attach ~left:name_col ~top:ext_row) () in
      let status_label = GMisc.label (* ~xpad:7 *)
	~packing:(ext_table#attach ~left:status_col ~top:ext_row) () in
      status_label#set_use_markup true;
      let using_label = GMisc.label
	~packing:(ext_table#attach ~left:using_col ~top:ext_row) () in
      using_label#set_use_markup true;
      let button = GButton.toggle_button
	~label:"mark"
	~packing:(ext_table#attach ~fill:`NONE ~left:button_col ~top:ext_row)
	() in
      ignore(button#connect#toggled 
	       ~callback:(self#mark_button_toggled button ext));
      next_row <- next_row + 1;
      let hbar = GMisc.separator `HORIZONTAL 
      	~packing:(ext_table#attach ~left:0 ~right:last_col ~top:next_row) () in
      next_row <- next_row + 1;
      Hashtbl.add ext_hash ext.evar_internal_name
	{ ext_table_ext = ext;
	  ext_table_row = ext_row;
	  ext_table_status_label = status_label;
	  ext_table_using_label = using_label;
	  ext_table_button = button;
	  ext_table_other = 
	    [| id_label#coerce; name_label#coerce; hbar#coerce; |]
	};
      ()
    in
    if Array.length no_ext_label_widgets <> 0
    then self#delete_no_ext_label;
    List.iter doit l;
    self#adjust_vbars next_row

  (** Destroy all widgets in a table line. Takes care of releasing the
      "mark" button if necessary.
  *)
  method private destroy_ext_line ext_table_line =
    ext_table_line.ext_table_status_label#destroy ();
    ext_table_line.ext_table_using_label#destroy ();
    ext_table_line.ext_table_button#set_active false;
    ext_table_line.ext_table_button#destroy ();
    Array.iter (fun w -> w#destroy ()) ext_table_line.ext_table_other


  (** Delete an existential from the table. Destroys the widgets,
      releases the "mark" button if necessary and updates {!ext_hash}.
  *)
  method private undo_delete ext =
    let tl = Hashtbl.find ext_hash ext.evar_internal_name in
    self#destroy_ext_line tl;
    Hashtbl.remove ext_hash ext.evar_internal_name;
    if tl.ext_table_row < next_row
    then next_row <- tl.ext_table_row


  (** Process pending addition or deletion requests and make the whole
      table up-to-date. Only one of {!to_add} and {!to_delete} can be
      non-nil.
  *)
  method update_ext_dialog =
    assert(to_add = [] || to_delete = []);
    List.iter (fun l -> self#process_fresh_existentials l) (List.rev to_add);
    to_add <- [];
    List.iter 
      (fun l -> List.iter (fun e -> self#undo_delete e) l)
      to_delete;
    to_delete <- [];
    self#update_existential_status;
    if next_row = start_row 
    then self#make_no_ext_label
      

  (** Fist part of table initialization. Fill the table by processing
      all existentials in the proof tree of the given root node. This
      function can be called several times for different root nodes.
  *)
  (* XXX this method, called when the existential dialog is initialized,
   * does a left-to-right depth-first traversal through the tree. When the
   * proof was build up in a different order there are probably strange
   * effects with undo and redo, because undo sets the next free line
   * pointer such that there are gaps in the table and redo will insert
   * from that next line pointer, potentially overwriting other entries.
   *)
  method fill_table_lines (nodes : proof_tree_element list) =
    let rec iter node =
      if node#fresh_existentials <> [] then begin
          (* Printf.fprintf (Util.debugc()) "EXTTAB node %s exts %s\n%!"
           *   (node#debug_name)
           *   (String.concat " " (List.map (fun e -> e.evar_internal_name)
           *                         node#fresh_existentials));
           *)
	  self#process_fresh_existentials node#fresh_existentials;
        end;
      List.iter iter node#children
    in
    List.iter iter nodes

  (** Second part of table initialization, to be called after all
      proof trees have been processed with {!fill_table_lines}.
  *)
  method finish_table_init =
    self#update_existential_status;
    if next_row = start_row
    then self#make_no_ext_label

  (** Update colors after the configuration has been updated. *)
  method configuration_updated =
    self#make_inst_labels;
    self#update_existential_status

  (** Clear the table for reuse. *)
  method clear_for_reuse =
    if Array.length no_ext_label_widgets = 0 then begin
      Hashtbl.iter (fun _ tl -> self#destroy_ext_line tl) ext_hash;
      Hashtbl.clear ext_hash;
      next_row <- start_row;
    end

  (** Callback for key events. Call the appropriate action for each
      key.
  *)
  method key_pressed_callback ev =
    match GdkEvent.Key.keyval ev with 
      | ks when 
	  (ks = GdkKeysyms._Q || ks = GdkKeysyms._q) 
	  && (List.mem `CONTROL (GdkEvent.Key.state ev))
	  -> 
	exit 0
      | ks when (ks = GdkKeysyms._Q || ks = GdkKeysyms._q)  -> 
	self#destroy (); true
      | ks when ks = GdkKeysyms._Up -> 
	scroll_adjustment table_v_adjustment (-1); true
      | ks when ks = GdkKeysyms._Down -> 
	scroll_adjustment table_v_adjustment 1; true

      | _ -> false

  (** Callback for the [changed] signal of the scrollbar adjustment,
      which is emitted when anything but the [value] changes. This
      callback scrolls the table to the bottom, if {!clamp_to_bottom}
      is [true].
  *)
  method scrollbar_changed_callback () =
    let a = table_v_adjustment in
    (* 
     * Printf.fprintf (debugc()) "scroll changed %.1f - %.1f up %.1f\n%!"
     *   a#value (a#value +. a#page_size) a#upper;
     *)
    if clamp_to_bottom then
      let upper = a#upper in
      a#clamp_page ~lower:upper ~upper:upper

  (** Callback for the [value-changed] signal of the scrollbar
      adjustment, which is emitted when the [value] changes. This
      callback remembers in {!clamp_to_bottom} whether the table was
      scrolled to the bottom.
  *)
  method scrollbar_value_changed_callback () =
    let a = table_v_adjustment in
    (* 
     * Printf.fprintf (debugc()) "scroll value   %.1f - %.1f up %.1f\n%!"
     *   a#value (a#value +. a#page_size) a#upper;
     *)
    if a#value +. a#page_size >= a#upper
    then clamp_to_bottom <- true
    else clamp_to_bottom <- false

  (** Make this configuration dialog visible. *)
  method present = top_window#present()

  (** Action for the Close button and the destroy signal. *)
  method destroy () =
    self#release_pressed_button;
    proof_window#existential_clear ();
    top_window#destroy();

  initializer
    self#make_inst_labels
end



(** Create a new dialog for existential variables. Creates the widget
    hierarchy, initializes the management object and registers all
    callbacks. The initial table fill must be initiated by the caller.
*)
let make_ext_dialog proof_window proof_name =
  let top_window = GWindow.window () in
  let top_v_box = GPack.vbox ~packing:top_window#add () in
  let _ext_title = GMisc.label
    ~line_wrap:true
    ~justify:`CENTER
    ~markup:("<big><b>Existential Variables: " ^ proof_name ^ "</b></big>")
    ~xpad:10 ~ypad:10
    ~packing:top_v_box#pack () in

  (****************************************************************************
   *
   * Scrollbar for table
   *
   ****************************************************************************)
  let scrolling_hbox = GPack.hbox 
    ~packing:(top_v_box#pack ~expand:true) () in
  let table_scrolling = GBin.scrolled_window 
    ~hpolicy:`NEVER ~vpolicy:`ALWAYS
    ~packing:(scrolling_hbox#pack ~expand:true ~fill:false) ()
  in
  let table_v_adjustment = table_scrolling#vadjustment in

  (****************************************************************************
   *
   * table of existentials
   *
   ****************************************************************************)
  let ext_table = GPack.table ~border_width:5
    ~packing:table_scrolling#add_with_viewport () in
  let id_col = 1 in
  let name_col = 3 in
  let status_col = 5 in
  let using_col = 7 in
  let button_col = 9 in 
  let last_col = button_col + 2 in
  let xpadding = 7 in
  let ypadding = 3 in
  let row = 0 in
  let _hbar = GMisc.separator `HORIZONTAL 
    ~packing:(ext_table#attach ~left:0 ~right:last_col ~top:row) () in
  let row = row + 1 in
  let bar_l = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:0 ~top:row ~bottom:(row + 1)) () in
  let _id_heading = GMisc.label ~markup:"<b>ID</b>" 
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:id_col ~top:row) () in
  let bar_1 = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(id_col + 1) ~top:row) () in
  let _name_heading = GMisc.label ~markup:"<b>Name</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:name_col ~top:row) () in
  let bar_2 = GMisc.separator `VERTICAL
  ~packing:(ext_table#attach ~left:(name_col + 1) ~top:row) () in
  let _status_heading = GMisc.label ~markup:"<b>Status</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:status_col ~top:row) () in
  let bar_3 = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(status_col + 1) ~top:row) () in
  let _using_heading = GMisc.label ~markup:"<b>Using</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:using_col ~top:row) () in
  let bar_4 = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(using_col + 1) ~top:row) () in
  let _button_heading = GMisc.label ~markup:"<b>Mark</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:button_col ~top:row) () in
  let bar_r = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(button_col + 1) 
		~top:row ~bottom:(row + 1)) () in
  let border_vbars = [| (bar_l#coerce, 0); 
			(bar_r#coerce, button_col + 1) |] in
  let inner_vbars = [| (bar_1#coerce, id_col + 1);
                       (bar_2#coerce, name_col + 1);
		       (bar_3#coerce, status_col + 1);
		       (bar_4#coerce, using_col + 1); |] in

  let row = row + 1 in
  let _hbar = GMisc.separator `HORIZONTAL 
    ~packing:(ext_table#attach ~left:0 ~right:last_col ~top:row) () in
  let row = row + 1 in

  (****************************************************************************
   *
   * compute size
   *
   ****************************************************************************)
  let context = ext_table#misc#pango_context in
  let layout = context#create_layout in
  Pango.Layout.set_text layout "X";
  let (_, char_height) = Pango.Layout.get_pixel_size layout in
  top_window#set_default_size ~width:0 
    ~height:(8 * char_height + 
	       int_of_float ((float_of_int !current_config.ext_table_lines)
			     *. 1.76 *. (float_of_int char_height)));


  (****************************************************************************
   *
   * bottom button box
   *
   ****************************************************************************)
  let button_box = GPack.hbox 
    ~spacing:5 ~border_width:5 ~packing:top_v_box#pack () in
  let show_current_button = GButton.button
    ~label:"Show current" ~packing:button_box#pack () in
  let close_button = GButton.button
    ~stock:`CLOSE ~packing:(button_box#pack ~from:`END) () in

  let ext_window = 
    new existential_variable_window proof_window top_window 
      ext_table table_v_adjustment border_vbars inner_vbars
      row id_col name_col status_col using_col button_col last_col
  in

  top_window#set_title "Existential Variables";
  ignore(table_v_adjustment#connect#changed 
	   ~callback:ext_window#scrollbar_changed_callback);
  ignore(table_v_adjustment#connect#value_changed 
	   ~callback:ext_window#scrollbar_value_changed_callback);
  ignore(top_window#event#connect#key_press 
	   ~callback:ext_window#key_pressed_callback);
  ignore(top_window#connect#destroy ~callback:ext_window#destroy);
  ignore(show_current_button#connect#clicked 
	   ~callback:proof_window#show_current_node);
  ignore(close_button#connect#clicked ~callback:ext_window#destroy);
  top_window#show ();

  ext_window
