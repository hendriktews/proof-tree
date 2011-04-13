(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 Hendrik Tews
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the
 * parent directories for more details.
 * 
 * $Id: draw_tree.ml,v 1.8 2011/04/13 10:47:08 tews Exp $
 *)


(** Layout and drawing of the elements of the proof tree *)


open Configuration
open Gtk_ext

type node_kind =
  | Proof_command
  | Turnstile

type branch_state_type = 
  | Unproven
  | CurrentNode
  | Current
  | Proven


let safe_and_set_gc drawable state =
  match state with
    | Unproven -> None
    | CurrentNode
    | Current ->
      let res = Some drawable#get_foreground in
      drawable#set_foreground (`NAME("brown"));
      res
    | Proven -> 
      let res = Some drawable#get_foreground in
      drawable#set_foreground (`NAME("blue"));
      res

let restore_gc drawable fc_opt = match fc_opt with
  | None -> ()
  | Some fc -> drawable#set_foreground (`COLOR fc)


class virtual ['a] doubly_linked_tree =
object 
  val mutable parent = None
  val mutable children = []

  method parent = parent
  method set_parent (p : 'a) = parent <- Some p
  method clear_parent = parent <- None

  method children = children
  method set_children (cs : 'a list) = 
    children <- cs

  method virtual children_changed : unit
end

(* 
 * let set_children parent children =
 *   parent#set_children children;
 *   List.iter (fun c -> c#set_parent parent) children;
 *   parent#children_changed
 *)

let add_child parent child =
  parent#set_children (parent#children @ [child]);
  child#set_parent parent;
  parent#children_changed

let remove_child child =
  match child#parent with
    | None -> ()
    | Some p -> 
      p#set_children (List.filter (fun c -> c <> child) p#children);
      child#clear_parent;
      p#children_changed

class virtual proof_tree_element (drawable : better_drawable) debug_name = 
object (self)
  inherit [proof_tree_element] doubly_linked_tree as super

  val debug_name = (debug_name : string)
  method private debug_name = debug_name

  method virtual node_kind : node_kind

  val drawable = drawable

  val mutable width = 0
  val mutable height = 0
  val mutable subtree_width = 0
  val mutable first_child_offset = 0
  val mutable x_offset = 0
  val mutable subtree_levels = 0
  val mutable branch_state = Unproven
  val mutable selected = false

  method width = width
  method height = height
  method subtree_width = subtree_width
  method subtree_levels = subtree_levels
  method x_offset = x_offset
  method branch_state = branch_state
  method set_branch_state s = branch_state <- s
  method selected b = selected <- b

  method virtual content : string
  method virtual id : string

  method private iter_children : 
    'a . int -> int -> 'a -> 
      (int -> int -> 'a -> proof_tree_element -> ('a * bool)) -> 'a =
    fun left top a f ->
    let left = left + first_child_offset in
    let top = top + !current_config.level_distance in
    let rec doit left a = function
      | [] -> a
      | c::cs -> 
	let (na, cont) = f left top a c in
	if cont
	then doit (left + c#subtree_width) na cs
	else na
    in
    doit left a children

  method private iter_all_children_unit left top 
    (f : int -> int -> proof_tree_element -> unit) =
    self#iter_children left top ()
      (fun left top () c -> f left top c; ((), true))

  method subtree_height = 
    (subtree_levels - 1) * !current_config.level_distance + 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width

  method private update_subtree_size =
    let (children_width, max_levels, last_child) = 
      List.fold_left 
	(fun (sum_width, max_levels, last_child) c -> 
	  (sum_width + c#subtree_width,
	   (if c#subtree_levels > max_levels 
	    then c#subtree_levels 
	    else max_levels),
	   Some c))
	(0, 0, None)
	children 
    in
    subtree_levels <- max_levels + 1;
    subtree_width <- children_width;
    x_offset <- 
      (match children with
	| [] -> 0
	| [c] -> c#x_offset
	| first :: _ -> match last_child with
	    | None -> assert false
	    | Some last -> 
	      let last_x_offset = 
		subtree_width - last#subtree_width + last#x_offset
	      in
	      (first#x_offset + last_x_offset) / 2
      );
    (* 
     * Printf.eprintf "USS %s childrens width %d first x_offset %d\n%!"
     *   self#debug_name
     *   children_width
     *   x_offset;
     *)
    (* Now x_offset is nicely in the middle of all children nodes and
     * subtree_width holds the width of all children nodes.
     * However, the width of this node might be larger than all the 
     * children together, or it may be placed asymmetrically. In both 
     * cases it can happen that some part of this node is outside the 
     * boundaries of all the children. In this case we must increase 
     * the width of subtree and adjust the x_offset.
     *)
    if x_offset < width / 2 
    then begin 
      (* part of this node is left of leftmost child *)
      first_child_offset <- width / 2 - x_offset;
      x_offset <- x_offset + first_child_offset;
      subtree_width <- subtree_width + first_child_offset;
    end else begin
      (* this node's left side is right of the left margin of the first child *)
      first_child_offset <- 0;
    end;
    if subtree_width - x_offset < width / 2 
    then begin
      (* Part of this node is right of rightmost child.
       * Need to increase subtree_width about the outside part, 
       * which is   width / 2 - (subtree_width - x_offset).
       * Now 
       *    subtree_width + width / 2 - (subtree_width - x_offset) =
       *      x_offset + width / 2
       *)
      subtree_width <- x_offset + width / 2;
    end else begin
      (* This node's right side is left of right margin of last child.
       * Nothing to do.
       *)
    end;
    (* 
     * Printf.eprintf 
     *   "USS %s END subtree width %d x_offset %d first_child_offset %d\n%!"
     *   self#debug_name
     *   subtree_width
     *   x_offset
     *   first_child_offset;
     *)
	
  method update_sizes_in_branch =
    (* 
     * let old_subtree_width = subtree_width in
     * let old_x_offset = x_offset in
     *)
    self#update_subtree_size;
    (* 
     * if x_offset <> old_x_offset || subtree_width <> old_subtree_width
     * then
     *)
      match parent with 
	| None -> ()
	| Some p -> p#update_sizes_in_branch

  method children_changed =
    (* prerr_endline("CHILDS at  " ^ self#debug_name ^ " CHANGED"); *)
    self#update_sizes_in_branch
    (* prerr_endline "END CHILD CHANGED" *)

  method child_offset child =
    self#iter_children 0 0 0 (fun left _top _a oc -> (left, child <> oc))

  method left_top_offsets =
    match parent with
      | None -> (0, 0)
      | Some p ->
	let (parent_left, parent_top) = p#left_top_offsets in
	let top_off = parent_top + !current_config.level_distance in
	let left_off = 
	  parent_left + p#child_offset (self :> proof_tree_element) 
	in
	(left_off, top_off)

  method x_y_offsets =
    let (left, top) = self#left_top_offsets in
    (left + x_offset, top + height / 2)

  method get_koordinates left top = (left + x_offset, top + height / 2)

  (* draw left top => unit *)
  method private virtual draw : int -> int -> unit

  (* line_offset inverse_slope => (x_off, y_off) *)
  method virtual line_offset : float -> (int * int)

  method private draw_lines left top =
    let (x, y) = self#get_koordinates left top in
    self#iter_all_children_unit left top
      (fun left top child ->
       let (cx, cy) = child#get_koordinates left top in
       let slope = float_of_int(cx - x) /. float_of_int(cy - y) in
       let (d_x, d_y) = self#line_offset slope in
       let (c_d_x, c_d_y) = child#line_offset slope in
       let child_state = child#branch_state in
       let line_state = match (branch_state, child_state) with
	 | (Unproven, Current)
	 | (Unproven, CurrentNode)
	 | (Proven, Unproven)
	 | (Proven, Current) 
	 | (Proven, CurrentNode) -> assert false
	 | (Unproven, Unproven)
	 | (Unproven, Proven) 
	 | ((Current|CurrentNode), Unproven)
	 | ((Current|CurrentNode), (Current|CurrentNode))
	 | ((Current|CurrentNode), Proven) 
	 | (Proven, Proven) -> child_state
       in
       let gc_opt = safe_and_set_gc drawable line_state in
       drawable#line ~x:(x + d_x) ~y:(y + d_y) 
	 ~x:(cx - c_d_x) ~y:(cy - c_d_y);
       restore_gc drawable gc_opt)

  method draw_subtree left top =
    (* 
     * Printf.eprintf "DST %s parent %s childs %s width %d tree_width %d\n%!"
     *   debug_name
     *   (match parent with
     * 	| None -> "None"
     * 	| Some p -> p#debug_name)
     *   (String.concat ", " (List.map (fun c -> c#debug_name) children))
     *   width
     *   subtree_width;
     *)
    let gc_opt = safe_and_set_gc drawable branch_state in
    self#draw left top;
    restore_gc drawable gc_opt;

    self#draw_lines left top;
    self#iter_all_children_unit left top 
      (fun left top child -> child#draw_subtree left top)

  method mouse_button_tree left top bx by =
    if bx >= left && bx <= left + subtree_width &&
      by >= top && by <= top + self#subtree_height
    then
      let (x,y) = self#get_koordinates left top in
      if bx >= x - width/2 && bx <= x + width/2 &&
	by >= y - height/2 && by <= y + height/2
      then
	Some (self :> proof_tree_element)
      else
	self#iter_children left top None
	  (fun left top _a child ->
	    let cres = child#mouse_button_tree left top bx by in
	    (cres, cres = None))
    else
      None

  method mark_branch (f : proof_tree_element -> bool) =
    if f (self :> proof_tree_element) then
      match parent with
	| Some p -> p#mark_branch f
	| None -> ()

  method mark_current =
    self#mark_branch 
      (fun (self : proof_tree_element) -> 
	if self#branch_state = Current 
	then false
	else
	  (self#set_branch_state Current; true));
    branch_state <- CurrentNode

  method mark_proved =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	if (List.for_all (fun c -> c#branch_state = Proven) self#children)
	then (self#set_branch_state Proven; 
	      (* Printf.eprintf "Mark %s proven\n%!" self#debug_name; *)
	      true)
	else false
      )

  method unmark_current =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	match self#branch_state with
	  | CurrentNode
	  | Current -> 
	    self#set_branch_state Unproven; true
	  | Unproven -> false
	  | Proven -> assert false
      )

  method unmark_proved =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	if self#branch_state = Proven
	then (self#set_branch_state Unproven; true)
	else false
      )

  method disconnect_proof =
    if branch_state = Current 
    then branch_state <- Unproven;
    List.iter (fun c -> c#disconnect_proof) children;
end



class turnstile (drawable : better_drawable) sequent_id sequent_text =
object (self)
  inherit proof_tree_element drawable sequent_id

  val mutable sequent_id = sequent_id
  val mutable sequent_text = (sequent_text : string)

  method node_kind = Turnstile

  method content = sequent_text
  method id = sequent_id
  method update_sequent new_text = sequent_text <- new_text

  method private draw_turnstile x y =
    let radius = !current_config.turnstile_radius in
    if branch_state = CurrentNode
    then
      drawable#arc ~x:(x - radius) ~y:(y - radius) 
	~width:(2 * radius) ~height:(2 * radius) ();
    (if selected 
     then
	let wh_2 = radius + !current_config.turnstile_line_width in
	drawable#rectangle 
	  ~x:(x - wh_2) ~y:(y - wh_2) ~width:(2 * wh_2) ~height:(2 * wh_2) ();
    );
    drawable#line 
      ~x:(x + !current_config.turnstile_left_bar_x_offset)
      ~y:(y - !current_config.turnstile_left_bar_y_offset)
      ~x:(x + !current_config.turnstile_left_bar_x_offset)
      ~y:(y + !current_config.turnstile_left_bar_y_offset);
    drawable#line
      ~x:(x + !current_config.turnstile_left_bar_x_offset)
      ~y
      ~x:(x + !current_config.turnstile_horiz_bar_x_offset)
      ~y

  method private draw left top =
    let (x, y) = self#get_koordinates left top in
    self#draw_turnstile x y

  method line_offset slope =
    let radius = !current_config.turnstile_radius + !current_config.line_sep in
    let d_y = sqrt(float_of_int(radius * radius) /. (slope *. slope +. 1.0)) in
    let d_x = slope *. d_y in
    (int_of_float(d_x +. 0.5), int_of_float(d_y +. 0.5))

  initializer
    width <- 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width +
      !current_config.subtree_sep;
    height <- 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width;
    self#update_subtree_size

end

class proof_command (drawable_arg : better_drawable) command debug_name =
object (self)
  inherit proof_tree_element drawable_arg debug_name

  val displayed_command =
    if Util.utf8_string_length command <= !current_config.proof_command_length
    then command
    else (Util.utf8_string_sub command 
	    (!current_config.proof_command_length - 1))
      ^ "\226\128\166" 			(* append horizontal ellipsis *)
  val command = command
  val layout = drawable_arg#pango_context#create_layout
  val mutable layout_width = 0
  val mutable layout_height = 0

  method node_kind = Proof_command
  method content = command
  method id = ""

  method private draw left top = 
    let (x, y) = self#get_koordinates left top in
    drawable#put_layout ~x:(x - layout_width/2) ~y:(y - layout_height/2) layout;
    if selected 
    then
      let w = layout_width + !current_config.turnstile_line_width in
      let h = layout_height + !current_config.turnstile_line_width in
      drawable#rectangle 
	~x:(x - w/2) ~y:(y - h/2) ~width:w ~height:h ();

  method line_offset slope = 
    let sign = if slope >= 0.0 then 1 else -1 in
    let line_sep = !current_config.line_sep in
    let corner_slope = (float_of_int width) /. (float_of_int height) in
    (* slope and corner_slope are actually inverse slopes: 
     * they are d_x / d_y. This is because d_y is guaranteed to be non_zero,
     * while d_x is not.
     *)
    if (abs_float slope) <= corner_slope
    then (* intersect with top or bottom *)
      (int_of_float(slope *. (float_of_int (height/2 + line_sep)) +. 0.5),
       height/2 + line_sep)
    else (* intersect with left or right side *)
      ((width/2 + line_sep) * sign,
       int_of_float(float_of_int(width/2 + line_sep) /. slope +. 0.5) * sign)

  initializer
    Pango.Layout.set_text layout displayed_command;
    let (w,h) = Pango.Layout.get_pixel_size layout in
    layout_width <- w;
    layout_height <- h;
    width <- w + !current_config.subtree_sep;
    height <- h;
    (* 
     * Printf.eprintf "INIT %s w %d width %d height %d\n%!"
     *   self#debug_name w width height;
     *)
    self#update_subtree_size

end
