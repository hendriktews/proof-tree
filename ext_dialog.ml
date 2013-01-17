(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011, 2012 Hendrik Tews
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
 * $Id: ext_dialog.ml,v 1.9 2013/01/17 07:48:04 tews Exp $
 *)

(** The Existential Variable Dialog *)

open Gtk_ext
open Configuration
open Draw_tree


(** Record for one line in the table of existential variables. This
    record links the {!Draw_tree.existential_variable} record and the
    necessary GTK widgets in order to update the table line when the
    status of the existential changes. For memory management is makes
    also sense to destroy all widgets when a line gets deleted.

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
    - start_row		first row in the table
    - name_col		column for names
    - status_col	column for status
    - button_col	column for buttons
    - last_col		last column
*)
class existential_variable_window
  proof_window
  (top_window : GWindow.window)
  (ext_table : GPack.table)
  start_row name_col status_col using_col button_col last_col
  =
object (self)

  (** "no" label text for the instantiated column. *)
  val uninst_label = "no"

  (** "partially" label text for the instantiated column. Contains
      pango markup for the color.
  *)
  val mutable partially_inst_label = ""

  (** "fully" label text for the instantiated column. Contains pango
      markup for the color.
  *)
  val mutable fully_inst_label = ""

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

  (** Put a "no existential variables" line into the table and store
      its widgets in {!no_ext_label_widgets}.
  *)
  method private make_no_ext_label =
    let hbar = GMisc.separator `HORIZONTAL 
      ~packing:(ext_table#attach ~left:0 ~right:last_col ~top:next_row) () in
    next_row <- next_row + 1;
    let no_ext_label = GMisc.label 
      ~text:"There are no existential variables" ~ypad:8
      ~packing:(ext_table#attach ~left:name_col ~right:last_col ~top:next_row)
      () in
    next_row <- next_row + 1;
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
    if existential.status <> Uninstantiated then
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
      existential.existential_mark <- true;
      (match self#get_ext_nodes existential with
	| (crea, Some inst) ->
	  proof_window#show_both_nodes crea inst
	| (crea, None) -> 
	  proof_window#show_node crea
      );
      proof_window#invalidate_drawing_area;
    end
    else begin
      existential.existential_mark <- false;
      pressed_mark_button <- None;
      proof_window#invalidate_drawing_area;
    end

  (** Update the information in the argument table line. *)
  method private set_ext_line_status tl =
    tl.ext_table_status_label#set_label
      (match tl.ext_table_ext.status with
	| Uninstantiated -> uninst_label
	| Partially_instantiated -> partially_inst_label
	| Fully_instantiated -> fully_inst_label
      );
    let color_ex_name ex =
      match ex.status with
	| Uninstantiated -> ex.existential_name
	| Partially_instantiated -> 
	  pango_markup_color ex.existential_name !proved_partial_gdk_color
	| Fully_instantiated -> 
	  pango_markup_color ex.existential_name !proved_complete_gdk_color
    in
    let colored_dep_names =
      String.concat ", " 
	(List.map color_ex_name tl.ext_table_ext.dependencies) in
    tl.ext_table_using_label#set_label colored_dep_names
      

  (** Update the status of the complete table. *)
  method private update_existential_status =
    Hashtbl.iter 
      (fun _ tl ->
	(* We don't have to update every line. However, the test is
	 * rather complicated, because we have to check whether the
	 * status of any of the dependencies changes. For that one
	 * would have to through ext_hash, in order to find the last
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
      assert(Hashtbl.mem ext_hash ext.existential_name = false);
      let hbar = GMisc.separator `HORIZONTAL 
      	~packing:(ext_table#attach ~left:0 ~right:last_col ~top:next_row) () in
      next_row <- next_row + 1;
      let name_label = GMisc.label ~text:ext.existential_name
	~packing:(ext_table#attach ~left:name_col ~top:next_row) () in
      let bar_1 = GMisc.separator `VERTICAL
	~packing:(ext_table#attach ~left:(name_col + 1) ~top:next_row) () in
      let status_label = GMisc.label (* ~xpad:7 *)
	~packing:(ext_table#attach ~left:status_col ~top:next_row) () in
      status_label#set_use_markup true;
      let bar_2 = GMisc.separator `VERTICAL
	~packing:(ext_table#attach ~left:(status_col + 1) ~top:next_row) () in
      let using_label = GMisc.label
	~packing:(ext_table#attach ~left:using_col ~top:next_row) () in
      using_label#set_use_markup true;
      let bar_3 = GMisc.separator `VERTICAL
	~packing:(ext_table#attach ~left:(using_col + 1) ~top:next_row) () in
      let button = GButton.toggle_button
	~label:"mark"
	~packing:(ext_table#attach ~fill:`NONE ~left:button_col ~top:next_row)
	() in
      ignore(button#connect#toggled 
	       ~callback:(self#mark_button_toggled button ext));
      Hashtbl.add ext_hash ext.existential_name 
	{ ext_table_ext = ext;
	  ext_table_row = next_row;
	  ext_table_status_label = status_label;
	  ext_table_using_label = using_label;
	  ext_table_button = button;
	  ext_table_other = 
	    [| name_label#coerce; bar_1#coerce; bar_2#coerce; bar_3#coerce;
	       hbar#coerce; |]
	};
      next_row <- next_row + 1;
      ()
    in
    if Array.length no_ext_label_widgets <> 0
    then self#delete_no_ext_label;
    List.iter doit l


  (** Destroy all widgets in a table line. Takes care of releasing the
      "mark" button if necessary.
  *)
  method private destroy_ext_line ext_table_line =
    ext_table_line.ext_table_status_label#destroy ();
    ext_table_line.ext_table_using_label#destroy ();
    ext_table_line.ext_table_button#set_active false;
    ext_table_line.ext_table_button#destroy ();
    Array.iter (fun w -> w#destroy ()) ext_table_line.ext_table_other;


  (** Delete an existential from the table. Destroys the widgets,
      releases the "mark" button if necessary and updates {!ext_hash}.
  *)
  method private undo_delete ext =
    let tl = Hashtbl.find ext_hash ext.existential_name in
    self#destroy_ext_line tl;
    Hashtbl.remove ext_hash ext.existential_name;
    if tl.ext_table_row < next_row
    (* Take the separator obove the row into account! *)
    then next_row <- tl.ext_table_row - 1


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
      

  (** XXX Initially fill the table by processing all existentials in the
      subtree of the argument node.
  *)
  method fill_table_lines (nodes : proof_tree_element list) =
    let rec iter node =
      if node#fresh_existentials <> [] then
	self#process_fresh_existentials node#fresh_existentials;
      List.iter iter node#children
    in
    List.iter iter nodes

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
   * table of existentials
   *
   ****************************************************************************)
  let table_hbox = GPack.hbox 
    ~packing:(top_v_box#pack ~expand:true ~fill:false) () in
  let table_frame = GBin.frame ~border_width:5
    ~packing:(table_hbox#pack ~expand:true ~fill:false) () in
  (* let table_frame = top_v_box in *)
  let ext_table = GPack.table ~border_width:5
    ~packing:table_frame#add () in
  let name_col = 0 in
  let status_col = 2 in
  let using_col = 4 in
  let button_col = 6 in 
  let last_col = button_col + 1 in
  let xpadding = 7 in
  let ypadding = 3 in
  let row = 0 in
  let _name_heading = GMisc.label ~markup:"<b>Name</b>" 
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:name_col ~top:row) () in
  let _bar = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(name_col + 1) ~top:row) () in
  let _status_heading = GMisc.label ~markup:"<b>Instantiated</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:status_col ~top:row) () in
  let _bar = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(status_col + 1) ~top:row) () in
  let _using_heading = GMisc.label ~markup:"<b>Using</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:using_col ~top:row) () in
  let _bar = GMisc.separator `VERTICAL
    ~packing:(ext_table#attach ~left:(using_col + 1) ~top:row) () in
  let _button_heading = GMisc.label ~markup:"<b>Mark Nodes</b>"
    ~xpad:xpadding ~ypad:ypadding
    ~packing:(ext_table#attach ~left:button_col ~top:row) () in

  let row = row + 1 in

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
    ~label:"Close" ~packing:(button_box#pack ~from:`END) () in

  let ext_window = 
    new existential_variable_window proof_window top_window ext_table 
      row name_col status_col using_col button_col last_col
  in

  top_window#set_title "Existential Variables";
  ignore(top_window#connect#destroy ~callback:ext_window#destroy);
  ignore(show_current_button#connect#clicked 
	   ~callback:proof_window#show_current_node);
  ignore(close_button#connect#clicked ~callback:ext_window#destroy);
  top_window#show ();

  ext_window
