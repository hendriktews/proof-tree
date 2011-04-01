open Util
open Gtk_ext

type configuration = {
  turnstile_radius : int;
  turnstile_left_bar_x_offset : int;
  turnstile_left_bar_y_offset : int;
  turnstile_horiz_bar_x_offset : int;
  turnstile_line_width : int;

  subtree_sep : int;
  line_sep : int;

  level_distance : int;
}


let update_sizes config radius =
  { config with 
      turnstile_radius = radius;
      turnstile_left_bar_x_offset = 
        int_of_float(-0.23 *. (float_of_int radius) +. 0.5);
      turnstile_left_bar_y_offset =
        int_of_float(0.65 *. (float_of_int radius) +. 0.5);
      turnstile_horiz_bar_x_offset =
        int_of_float(0.7 *. (float_of_int radius) +. 0.5);

      level_distance = 4 * radius
  }

let default_configuration = 
  let radius = 15 in
  let c = {
    turnstile_radius = radius;
    turnstile_line_width = 2;
    subtree_sep = 10;
    line_sep = 3;

    turnstile_left_bar_x_offset = 0;
    turnstile_left_bar_y_offset = 0;
    turnstile_horiz_bar_x_offset = 0;
    level_distance = 0;
  }
  in
  update_sizes c radius

let current_config = ref default_configuration
 
class virtual proof_tree_element (drawable : better_drawable) debug_name = 
object (self)
  inherit [proof_tree_element] doubly_linked_tree as super

  val debug_name = (debug_name : string)
  method debug_name = debug_name

  val drawable = drawable

  val mutable width = 0
  val mutable height = 0
  val mutable subtree_width = 0
  val mutable first_child_offset = 0
  val mutable x_offset = 0
  val mutable subtree_levels = 0

  method height = height
  method subtree_width = subtree_width
  method subtree_levels = subtree_levels
  method x_offset = x_offset

  method iter_children left top (f : int -> int -> proof_tree_element -> unit) =
    let left = left + first_child_offset in
    let top = top + !current_config.level_distance in
    ignore(
      List.fold_left
	(fun left child -> 
	  f left top child;
	  left + child#subtree_width)
	left
	children)
    
  method subtree_height = 
    (subtree_levels - 1) * !current_config.level_distance + 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width

  method get_x_position left (children_koord_rev : (int * int * int) list) = 
    match children_koord_rev with
      | [] -> left + width / 2
      | [(child_koord_x, _, _)] -> child_koord_x
      | (last_x, _, _) :: rest ->
	let (first_x, _, _) = list_last rest in
	(first_x + last_x) / 2

  method update_subtree_size =
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

  method get_koordinates left top = (left + x_offset, top + height / 2)

  (* draw left top => unit *)
  method virtual draw : int -> int -> unit

  method virtual line_offset : float -> (int * int)

  method draw_lines left top =
    let (x, y) = self#get_koordinates left top in
    self#iter_children left top
      (fun left top child ->
       let (cx, cy) = child#get_koordinates left top in
       let slope = float_of_int(cx - x) /. float_of_int(cy - y) in
       let (d_x, d_y) = self#line_offset slope in
       let (c_d_x, c_d_y) = child#line_offset slope in
       drawable#line ~x:(x + d_x) ~y:(y + d_y) 
	 ~x:(cx - c_d_x) ~y:(cy - c_d_y))

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
    self#draw left top;
    self#draw_lines left top;
    self#iter_children left top 
      (fun left top child -> child#draw_subtree left top)

  method virtual mouse_button_1 : unit

  method mouse_button button = 
    (* Printf.eprintf "%s Button %d\n%!" self#debug_name button; *)
    match button with
      | 1 -> self#mouse_button_1
      | _ -> ()

  method mouse_button_tree left top bx by button =
    if bx >= left && bx <= left + subtree_width &&
      by >= top && by <= top + self#subtree_height
    then
      let (x,y) = self#get_koordinates left top in
      if bx >= x - width/2 && bx <= x + width/2 &&
	by >= y - height/2 && by <= y + height/2
      then
	self#mouse_button button
      else
	self#iter_children left top
	  (fun left top child ->
	    child#mouse_button_tree left top bx by button)
end

class turnstile (drawable : better_drawable) debug_name =
object (self)
  inherit proof_tree_element drawable debug_name

  method draw_turnstile x y =
    let radius = !current_config.turnstile_radius in
    drawable#set_line_attributes 
      ~width:(!current_config.turnstile_line_width) ();
    drawable#arc ~x:(x - radius) ~y:(y - radius) 
      ~width:(2 * radius) ~height:(2 * radius) ();
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

  method draw left top =
    let (x, y) = self#get_koordinates left top in
    self#draw_turnstile x y

  method line_offset slope =
    let radius = !current_config.turnstile_radius + !current_config.line_sep in
    let d_y = sqrt(float_of_int(radius * radius) /. (slope *. slope +. 1.0)) in
    let d_x = slope *. d_y in
    (int_of_float(d_x +. 0.5), int_of_float(d_y +. 0.5))

  method mouse_button_1 =
    Printf.eprintf "%s Button1\n%!" self#debug_name;

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

  val layout = drawable_arg#pango_context#create_layout
  val mutable layout_width = 0
  val mutable layout_height = 0

  initializer
    Pango.Layout.set_text layout command;
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

  method draw left top = 
    let (x, y) = self#get_koordinates left top in
    drawable#put_layout ~x:(x - layout_width/2) ~y:(y - layout_height/2) layout;

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

  method mouse_button_1 =
    Printf.eprintf "%s Button1\n%!" self#debug_name;

end
