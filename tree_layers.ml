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
 * $Id: tree_layers.ml,v 1.2 2013/01/17 09:57:07 tews Exp $
 *)


(** Organize several layers of proof trees *)

open Util
open Configuration
open Draw_tree
open Ext_dialog


class tree_layer tree_list = object (self)

  val tree_list = (tree_list : proof_tree_element list)

  val mutable width = None

  val mutable height = None

  val mutable layer_stack = (None : tree_layer abstract_tree_container option)

  initializer
    assert (tree_list <> []);
    List.iter 
      (fun r -> 
	r#register_tree_layer 
	  (self :> proof_tree_element abstract_tree_container))
      tree_list

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

  method left_top_offset =
    match layer_stack with
      | None -> assert false
      | Some ls ->
	let (ls_left, ls_top) = ls#left_top_offset in
	let (me_left, me_top) = ls#child_offsets (self :> tree_layer) in
	(ls_left + me_left, ls_top + me_top)

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

  method width = 
    if width = None then self#update_size_info;
    match width with
      | Some w -> w
      | None -> assert false

  method height = 
    if height = None then self#update_size_info;
    match height with 
      | Some h -> h
      | None -> assert false

  method private clear_self_size_cache =
    width <- None;
    height <- None

  method clear_size_cache =
    self#clear_self_size_cache;
    match layer_stack with
      | None -> assert false
      | Some sco -> sco#clear_size_cache

  method register_size_cache sco =
    assert (layer_stack = None);
    layer_stack <- Some sco

  method survive_undo_before_start_hint = match tree_list with
    | [] -> assert false
    | first :: _ -> first#children <> []

  method get_first_root = match tree_list with
    | [] -> assert false
    | first :: _ -> Some first

  method disconnect =
    List.iter (fun root -> root#disconnect_proof) tree_list

  method configuration_updated =
    self#clear_self_size_cache;
    List.iter (fun root -> root#configuration_updated) tree_list

  method update_sequent_existentials_info =
    List.iter (fun root -> root#update_existentials_info) tree_list

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

  method init_ext_dialog (ext : existential_variable_window) =
    ext#fill_table_lines tree_list

  method draw left top =
    ignore(
      List.fold_left
	(fun left r -> 
	  r#draw_tree_root left top;
	  left + !current_config.proof_tree_sep + r#subtree_width)
	left tree_list)

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

  method clone_layer new_pc new_seq ex_hash old_selected cloned_selected =
    let cloned_trees = 
      List.map 
	(clone_proof_tree new_pc new_seq ex_hash old_selected cloned_selected)
	tree_list
    in
    new tree_layer cloned_trees
end




class tree_layer_stack = object (self)

  val mutable layers = ([] : tree_layer list)

  val mutable width = None

  val mutable height = None

  method clear_size_cache =
    width <- None;
    height <- None

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

  method left_top_offset = (0, 0)

  method private layer_indent l = (self#width - l#width) / 2

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

  method width = 
    if width = None then self#update_size_info;
    match width with
      | Some w -> w
      | None -> assert false

  method height = 
    if height = None then self#update_size_info;
    match height with 
      | Some h -> h
      | None -> assert false

  method add_layer l =
    layers <- layers @ [l];
    l#register_size_cache (self :> tree_layer abstract_tree_container);
    self#clear_size_cache;
    List.length layers - 1

  method del_layer n = 
    layers <- firstn n layers

  method clear_for_reuse = layers <- []

  method set_layers ls = 
    layers <- ls;
    List.iter 
      (fun l -> 
	l#register_size_cache (self :> tree_layer abstract_tree_container))
      ls

  method count_layers = List.length layers

  method get_root_node = match layers with
    | [] -> None
    | first :: _ -> first#get_first_root

  method survive_undo_before_start_hint =
    match layers with
      | [] -> false
      | first :: _ -> first#survive_undo_before_start_hint

  method disconnect =
    List.iter (fun l -> l#disconnect) layers

  method configuration_updated = 
    self#clear_size_cache;
    List.iter (fun l -> l#configuration_updated) layers

  method update_sequent_existentials_info =
    List.iter (fun l -> l#update_sequent_existentials_info) layers

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

  method init_ext_dialog ext = 
    List.iter (fun l -> l#init_ext_dialog ext) layers

  method draw left top =
    ignore(
      List.fold_left
	(fun top l -> 
	  l#draw (left + self#layer_indent l) top;
	  top + !current_config.layer_sep + l#height)
	top layers)

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

  method clone_layers new_pc new_seq old_selected cloned_selected =
    let ex_hash = Hashtbl.create 251 in
    List.map 
      (fun l -> l#clone_layer new_pc new_seq 
	                      ex_hash old_selected cloned_selected)
      layers
end
