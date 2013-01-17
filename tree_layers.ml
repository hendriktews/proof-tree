(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2013 Hendrik Tews
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
 * $Id: tree_layers.ml,v 1.3 2013/01/17 14:39:12 tews Exp $
 *)


(** Organize several layers of proof trees *)

open Util
open Configuration
open Draw_tree
open Ext_dialog


(** Container for several independent proof trees in one horizontal layer.
 *)
class tree_layer tree_list = object (self)

  (***************************************************************************)
  (***************************************************************************)
  (** {2 State and accessors} *)
  (***************************************************************************)
  (***************************************************************************)

  (** The list of proof trees in this horizontal layer. *)
  val tree_list = (tree_list : proof_tree_element list)

  (** The width of this layer. Lazily computed in {!update_size_info}. *)
  val mutable width = None

  (** The height of this layer. Lazily computed in {!update_size_info}. *)
  val mutable height = None

  (** Upward pointer to the {!tree_layer_stack} that contains this
      layer. Needed position information. Must be set after
      initialization.
  *)
  val mutable layer_stack = (None : tree_layer abstract_tree_container option)

  (** Register the {!tree_layer_stack} that contains this layer. *)
  method register_layer_stack ls =
    assert (layer_stack = None);
    layer_stack <- Some ls

  (** The initializer ensures all proof trees have their upward
      pointer set.
  *)
  initializer
    assert (tree_list <> []);
    List.iter 
      (fun r -> 
	r#register_tree_layer 
	  (self :> proof_tree_element abstract_tree_container))
      tree_list


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Size and position} *)
  (***************************************************************************)
  (***************************************************************************)

  (** Recompute {!attribute: width} and {!attribute: height} *)
  method private update_size_info =
    let w = match tree_list with
      | [] -> assert false
      | [r] -> r#subtree_width
      | first :: rest ->
	List.fold_left
	  (fun width r -> 
	    width + !current_config.proof_tree_sep + r#subtree_width)
	  first#subtree_width
	  rest
    in 
    let h = 
      List.fold_left (fun height r -> max height r#subtree_height)
	0 tree_list
    in
    width <- Some w;
    height <- Some h

  (** Compute the left and top offset of this layer relative to the
      upper-left corner of the complete display. 
  *)
  method left_top_offset =
    match layer_stack with
      | None -> assert false
      | Some ls ->
	let (ls_left, ls_top) = ls#left_top_offset in
	let (me_left, me_top) = ls#child_offsets (self :> tree_layer) in
	(ls_left + me_left, ls_top + me_top)

  (** Compute the x and y offset of one child relative to the upper
      left corner of this layer.
  *)
  method child_offsets root =
    let root_left = ref None in
    (try
       ignore(
	 List.fold_left
	   (fun left oroot ->
	     if root = oroot 
	     then begin
	       root_left := Some left;
	       raise Exit
	     end else
	       left + !current_config.proof_tree_sep + oroot#subtree_width)
	   0 tree_list)
     with Exit -> ()
    );
    match !root_left with
      | None -> assert false
      | Some left -> (left, 0)

  (** Width of this layer. Recompute if necessary. *)
  method width = 
    if width = None then self#update_size_info;
    match width with
      | Some w -> w
      | None -> assert false

  (** Height of this layer. Recompute if necessary. *)
  method height = 
    if height = None then self#update_size_info;
    match height with 
      | Some h -> h
      | None -> assert false

  (** Clear the cached size information for this layer. *)
  method private clear_self_size_cache =
    width <- None;
    height <- None

  (** Invalidate the size information here and in those objects
      containing this layer.
  *)
  method clear_size_cache =
    self#clear_self_size_cache;
    match layer_stack with
      | None -> assert false
      | Some sco -> sco#clear_size_cache

  (** Draw the content of this layer relative to the specified left
      and top coordinate.
  *)
  method draw left top =
    ignore(
      List.fold_left
	(fun left r -> 
	  r#draw_tree_root left top;
	  left + !current_config.proof_tree_sep + r#subtree_width)
	left tree_list)

  (** Find the proof tree node at coordinates [(bx, by)] or return
      [None].
  *)
  method find_node_for_point_in_layer left top bx by =
    let rec iter left = function
      | [] -> None
      | r :: rest ->
	if left <= bx && bx <= left + r#subtree_width
	then r#find_node_for_point_root left top bx by
	else 
	  let left = 
	    left + !current_config.proof_tree_sep + r#subtree_width in
	  if left <= bx
	  then iter left rest
	  else None
    in
    iter left tree_list


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Misc} *)
  (***************************************************************************)
  (***************************************************************************)

  (** Give a hint if the proof-tree window containing this layer
      should survive an undo before the start of the proof.
  *)
  method survive_undo_before_start_hint = match tree_list with
    | [] -> assert false
    | first :: _ -> first#children <> []

  (** Return the first proof goal. Needed for selecting the initial
      goal after proof completion.
  *)
  method get_first_root = match tree_list with
    | [] -> assert false
    | first :: _ -> Some first

  (** Disconnect this layer from Proof General. *)
  method disconnect =
    List.iter (fun root -> root#disconnect_proof) tree_list

  (** Process an updated configuration. *)
  method configuration_updated =
    self#clear_self_size_cache;
    List.iter (fun root -> root#configuration_updated) tree_list

  (** Update the information about existential variables in sequent
      displays belonging to nodes of this layer.
  *)
  method update_sequent_existentials_info =
    List.iter (fun root -> root#update_existentials_info) tree_list

  (** Find a node satisfying the predicate or return [None]. *)
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
    (try
       List.iter iter tree_list
     with Exit -> ()
    );
    !res

  (** Initialize the given existential variable dialog with all
      existentials occurring in nodes of this layer.
  *)
  method init_ext_dialog (ext : existential_variable_window) =
    ext#fill_table_lines tree_list

  (** Clone this layer. *)
  method clone_layer new_pc new_seq ex_hash old_selected cloned_selected =
    let cloned_trees = 
      List.map 
	(clone_proof_tree new_pc new_seq ex_hash old_selected cloned_selected)
	tree_list
    in
    new tree_layer cloned_trees
end





(** Container for several layers of proof trees. *)
class tree_layer_stack = object (self)

  (***************************************************************************)
  (***************************************************************************)
  (** {2 State and accessors} *)
  (***************************************************************************)
  (***************************************************************************)

  (** The layers in this stack. *)
  val mutable layers = ([] : tree_layer list)

  (** The width of this layer. Lazily computed in {!update_size_info}. *)
  val mutable width = None

  (** The height of this layer. Lazily computed in {!update_size_info}. *)
  val mutable height = None

  (** Add a new layer at the bottom. Return a suitable [n], such that
      {!del_layer}[ n] will delete this added layer.
  *)
  method add_layer l =
    layers <- layers @ [l];
    l#register_layer_stack (self :> tree_layer abstract_tree_container);
    self#clear_size_cache;
    List.length layers - 1

  (** Keep only the first [n] layers, deleting the following ones. 
  *)
  method del_layer n = 
    layers <- firstn n layers

  (** Prepare this layer stack for reuse. *)
  method clear_for_reuse = layers <- []

  (** Set the layers for this stack. *)
  method set_layers ls = 
    layers <- ls;
    List.iter 
      (fun l -> 
	l#register_layer_stack (self :> tree_layer abstract_tree_container))
      ls

  (** Return the number of layers. *)
  method count_layers = List.length layers

  (***************************************************************************)
  (***************************************************************************)
  (** {2 Size and position} *)
  (***************************************************************************)
  (***************************************************************************)

  (** Recompute {!attribute: width} and {!attribute: height} *)
  method private update_size_info =
    let w = List.fold_left (fun width l -> max width l#width) 0 layers in
    let h = match layers with
      | [] -> 0
      | [l] -> l#height
      | first :: rest ->
	List.fold_left
	  (fun height l -> height + !current_config.layer_sep + l#height)
	  first#height
	  rest
    in 
    width <- Some w;
    height <- Some h

  (** Compute the left and top offset of this layer relative to the
      upper-left corner of the complete display, which is trivial.
  *)
  method left_top_offset = (0, 0)

  (** Compute the indentation of the given layer. *)
  method private layer_indent l = (self#width - l#width) / 2

  (** Compute the x and y offset of one child relative to the upper
      left corner of this layer
  *)
  method child_offsets layer =
    let layer_top = ref None in
    (try
       ignore(
	 List.fold_left
	   (fun top olayer ->
	     if layer = olayer
	     then begin
	       layer_top := Some (self#layer_indent layer, top);
	       raise Exit
	     end else
	       top + !current_config.layer_sep + olayer#height)
	   0 layers)
     with Exit -> ()
    );
    match !layer_top with
      | None -> assert false
      | Some (left, top) -> (left, top)

  (** Invalidate the size information. *)
  method clear_size_cache =
    width <- None;
    height <- None

  (** Width of this layer. Recompute if necessary. *)
  method width = 
    if width = None then self#update_size_info;
    match width with
      | Some w -> w
      | None -> assert false

  (** Height of this layer. Recompute if necessary. *)
  method height = 
    if height = None then self#update_size_info;
    match height with 
      | Some h -> h
      | None -> assert false

  (** Draw the content of this stack of layers relative to the
      specified left and top coordinate. 
  *)
  method draw left top =
    ignore(
      List.fold_left
	(fun top l -> 
	  l#draw (left + self#layer_indent l) top;
	  top + !current_config.layer_sep + l#height)
	top layers)

  (** Find the proof tree node at coordinates [(bx, by)] or return
      [None].
  *)
  method find_node_for_point_in_layer_stack left top bx by =
    let rec iter top = function
      | [] -> None
      | l :: rest ->
	let left = left + (self#layer_indent l) in
	if top <= by && by <= top + l#height &&
	  left <= bx && bx <= left + l#width
	then l#find_node_for_point_in_layer left top bx by
	else 
	  let top = top + !current_config.layer_sep + l#height in
	  if top <= by
	  then iter top rest
	  else None
    in
    iter top layers


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Misc} *)
  (***************************************************************************)
  (***************************************************************************)

  (** Get the root node of the first layer. Needed for selecting the
      initial goal after proof completion. 
  *)
  method get_root_node = match layers with
    | [] -> None
    | first :: _ -> first#get_first_root

  (** Give a hint if the proof-tree window with this stack should
      survive an undo before the start of the proof. 
  *)
  method survive_undo_before_start_hint =
    match layers with
      | [] -> false
      | first :: _ -> first#survive_undo_before_start_hint

  (** Disconnect from Proof General. *)
  method disconnect =
    List.iter (fun l -> l#disconnect) layers

  (** Process an updated configuration. *)
  method configuration_updated = 
    self#clear_size_cache;
    List.iter (fun l -> l#configuration_updated) layers

  (** Update the information about existential variables in sequent
      displays belonging to nodes of this stack of layers.
  *)
  method update_sequent_existentials_info =
    List.iter (fun l -> l#update_sequent_existentials_info) layers

  (** Find a node satisfying the predicate or return [None]. *)
  method find_node p =
    let res = ref None in
    (try
       List.iter
	 (fun l -> match l#find_node p with
	   | None -> ()
	   | Some n ->
	     res := Some n;
	     raise Exit
	 ) layers
     with Exit -> ()
    );
    !res

  (** Initialize the given existential variable dialog with all
      existentials occurring in nodes of this layer.
  *)
  method init_ext_dialog ext = 
    List.iter (fun l -> l#init_ext_dialog ext) layers

  (** Clone this stack of layers. *)
  method clone_layers new_pc new_seq old_selected cloned_selected =
    let ex_hash = Hashtbl.create 251 in
    List.map 
      (fun l -> l#clone_layer new_pc new_seq 
	ex_hash old_selected cloned_selected)
      layers
end
