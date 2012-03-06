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
 * $Id: ext_dialog.ml,v 1.5 2012/03/06 14:57:45 tews Exp $
 *)

(** The Existential Variable Dialog *)

open Gtk_ext
open Configuration
open Draw_tree


type ext_table_line = {
  ext_table_ext : existential_variable;
  ext_table_row : int;
  ext_table_status_label : GMisc.label;
  ext_table_using_label : GMisc.label;
  ext_table_button : GButton.toggle_button;
  ext_table_other : GObj.widget array;
}


(** Class for managing dialogs for existential variables. Objects are
    created when the widget tree is completely constructed. Contains
    the necessary state and methods to handle all callbacks. The
    callbacks must be set up by the function that creates objects.

    Arguments are
    - proof_window      {!class: Proof_window.proof_window} to which this 
			window is associated to
    - top_window	{!GWindow.window} of the top-level widget
    - ext_table		{!GPack.table} of the main table
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

  val uninst_label = "no"

  val mutable partially_inst_label = ""

  val mutable fully_inst_label = ""

  val mutable next_row = start_row

  val ext_hash = Hashtbl.create 251

  val mutable to_add = []

  val mutable to_delete = []

  val mutable no_ext_label_widgets = [| |]

  val mutable pressed_mark_button = None

  method make_inst_labels =
    partially_inst_label <- 
      pango_markup_color "partially" !proved_partial_gdk_color;
    fully_inst_label <- 
      pango_markup_color "fully" !proved_complete_gdk_color

  method change_and_add new_ext =
    assert(to_delete = []);
    if new_ext <> [] then
      to_add <- new_ext :: to_add

  method change_and_delete remove_ext =
    assert(to_add = []);
    if remove_ext <> [] then
      to_delete <- remove_ext :: to_delete

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

  method private delete_no_ext_label =
    Array.iter (fun w -> w#destroy ()) no_ext_label_widgets;
    no_ext_label_widgets <- [| |];
    next_row <- start_row

  method release_pressed_button =
    (match pressed_mark_button with
      | None -> ()
      | Some button -> 
	button#set_active false;
	pressed_mark_button <- None
    );

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

  method private process_fresh_existentials l =
    (* this method is only called with nonempty l *)
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


  method private destroy_ext_line ext_table_line =
    ext_table_line.ext_table_status_label#destroy ();
    ext_table_line.ext_table_using_label#destroy ();
    ext_table_line.ext_table_button#set_active false;
    ext_table_line.ext_table_button#destroy ();
    Array.iter (fun w -> w#destroy ()) ext_table_line.ext_table_other;


  method private undo_delete ext =
    let tl = Hashtbl.find ext_hash ext.existential_name in
    self#destroy_ext_line tl;
    Hashtbl.remove ext_hash ext.existential_name;
    if tl.ext_table_row < next_row
    (* Take the separator obove the row into account! *)
    then next_row <- tl.ext_table_row - 1


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
      

  method fill_table (node : proof_tree_element) =
    let rec iter node =
      self#process_fresh_existentials node#fresh_existentials;
      List.iter iter node#children
    in
    iter node;
    self#update_existential_status;
    if next_row = start_row
    then self#make_no_ext_label

  method configuration_updated =
    self#make_inst_labels;
    self#update_existential_status

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
    callbacks.
*)
let make_ext_dialog proof_window proof_name =
  let top_window = GWindow.window () in
  let top_v_box = GPack.vbox ~packing:top_window#add () in
  let _config_title_1 = GMisc.label
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
