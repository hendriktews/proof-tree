(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2016 Hendrik Tews
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


(** Layout and drawing of the elements of the proof tree.

    Internally a proof tree is organized as an n-ary tree, where the
    nodes are proof goals and proof commands and the vertices connect
    them appropriately. This module is responsible for manipulating
    and displaying these trees and for locating nodes (e.g., on mouse
    clicks). 

    A real proof tree has a number of properties, about which this
    module is completely ignorant. For instance, the root node is
    always a proof goal; proof goal nodes have zero or more successor
    nodes, all of which are proof commands; and, finally, every proof
    command has precisely one proof-goal successor. These properties
    are neither assumed nor checked, they hopefully hold, because the
    tree is created in the right way.

    The common code of both proof-goal and proof-command nodes is in
    the class {!class: Draw_tree.proof_tree_element}. The class for proof goals,
    {!turnstile} and the class {!class: proof_command} are derived from it.
    To work around the impossible down-casts, {!proof_tree_element}
    contains some virtual method hooks for stuff that is really
    specific for just one of its subclasses.

    The tree layout functionallity has been designed such that its
    running time is independent of the size of the complete tree. When
    a new node is inserted into the tree, only its direct and indirect
    parent nodes need to recompute their layout data. No sibling node
    must be visited. To achieve this the nodes do not store absolut
    positions. Instead, nodes only store the width and height of
    themselves and of their subtrees. 

    Adjusting the tree layout when new elements are inserted works
    bottom up. Drawing the tree or looking up nodes (for mouse events)
    works top down. Therefore the nodes are organized in a
    doubly-linked tree, where children nodes contain a link to their
    parent. The doubly-linked tree functionality is in
    {!class: doubly_linked_tree}. 

*)


open Util
open Configuration
open Gtk_ext


(** {2 Utility types and functions} *)

(*****************************************************************************)
(*****************************************************************************)
(** {3 Existential variables} *)
(*****************************************************************************)
(*****************************************************************************)

(** The code for marking and displaying existential variables depends
    on proper sharing of these records: For each proof-tree window
    there must only be one record for each existential variable. The
    same existential variable in different (cloned) proof trees must
    have exactly one record for each proof-tree window.

    The proof-tree record ({!Proof_tree.proof_tree}) contains a hash
    table containing all existential variables for a given proof.
    Changing the state of an existental variable and marking one in
    the proof-tree display works by side effect: All proof tree nodes
    refer to the very same instance and therefore see the state
    change.

    Sets of existential variables are stored as lists, whoose order is
    usually not important. Therefore most functions that manipulate
    lists of existential variables do not preserve the order.
*)


(** Status of an existential variable. The tree of existentials is
    only scanned for redisplay. Therefore, a fully instantiated
    existential might have state [Partially_instantiated] until the next scan.
*)
type existential_status =
  | Uninstantiated			(** open, not instantiated *)
  | Partially_instantiated		(** instantiated, but the
					    instantiation uses some
					    existentials that are still open *)
  | Fully_instantiated			(** fully instantiated *)


(** Representation of existential variables. The [status] field is
    lazily updated in {!Proof_tree.update_existential_status}.
    Therefore, a fully instantiated existential might have status
    {!existential_status.Partially_instantiated} for some time.
*)
type existential_variable = {
  existential_name : string;		(** The name *)
  mutable status : existential_status;	(** instantiation status *)
  mutable existential_mark : bool;	(** [true] if this existential should 
					    be marked in the proof-tree
					    display *)
  mutable dependencies : existential_variable list;
                                        (** The list of evars that are used 
					    in the instantiation, 
					    if instantiated *)
}


(** Filter the non-instantiated existentials from the argument. 
*)
let filter_uninstantiated exl =
  list_filter_rev (fun ex -> ex.status = Uninstantiated) exl

(** Filter the partially instantiated existentials from the argument *)
let filter_partially_instantiated exl =
  list_filter_rev (fun ex -> ex.status = Partially_instantiated) exl


(** Derive the existential status for drawing a node or a connection
    line in the proof tree. 
*)
let combine_existential_status_for_tree exl =
  if List.for_all (fun ex -> ex.status = Fully_instantiated) exl
  then Fully_instantiated
  else if List.exists (fun ex -> ex.status = Uninstantiated) exl
  then Uninstantiated
  else Partially_instantiated


(** Convert a set of existential variables into a single string for
    display purposes.
*)
let string_of_existential_list exl =
  String.concat " " (List.map (fun ex -> ex.existential_name) exl)


(*****************************************************************************)
(*****************************************************************************)
(** {3 Misc types} *)
(*****************************************************************************)
(*****************************************************************************)

(** Kind of nodes in the proof-tree display. The two kinds correspond
    to the two subclasses {!proof_command} and {!turnstile} of
    {!proof_tree_element}.
*)
type node_kind =
  | Proof_command			(** proof command *)
  | Turnstile				(** sequent *)


(** Proof state of a node in the proof-tree display. *)
type branch_state_type = 
  | Unproven				(** no finished yet *)
  | CurrentNode				(** current sequent in prover *)
  | Current				(** on the path from the current 
					    sequent to the root of the tree *)
  | Cheated				(** proved, but with a cheating 
					    command *)
  | Proven				(** proved *)


(* 
 * write doc when used
 * let string_of_branch_state = function
 *   | Unproven    -> "Unproven"
 *   | CurrentNode	-> "CurrentNode"
 *   | Current	-> "Current"
 *   | Cheated     -> "Cheated"
 *   | Proven      -> "Proven"
 *)


(*****************************************************************************)
(*****************************************************************************)
(** {3 Graphics context color manipulations} *)
(*****************************************************************************)
(*****************************************************************************)

(** The following functions implement a simple save/restore feature
    for the forground color of the graphics context. A saved state is
    a color option. The value [None] means that the foreground color
    has not been changed and that there is therefore no need to
    restore it.
*)

(** Save the current foreground color in a value suitable for
    {!restore_gc}.
*)
let save_gc drawable =
  Some drawable#get_foreground


(** Restore the saved foreground color. Do nothing if the foreground
    color has not been changed.
*)
let restore_gc drawable fc_opt = match fc_opt with
  | None -> ()
  | Some fc -> drawable#set_foreground (`COLOR fc)


(** [save_and_set_gc drawable state existentials] sets the foreground
    color to one of the configured colors, depending on [state] and
    [existentials]. The function returns a value suitable for
    {!restore_gc} to restore the old foreground color.
*)
let save_and_set_gc drawable state existentials =
  (* 
   * if List.exists (fun e -> e.existential_mark) existentials
   * then begin
   *   let res = save_gc drawable in
   *   drawable#set_foreground (`COLOR !mark_subtree_gdk_color);
   *   res
   * end else
   *)
  match state with
    | Unproven -> None
    | CurrentNode
    | Current ->
      let res = save_gc drawable in
      drawable#set_foreground (`COLOR !current_gdk_color);
      res
    | Proven -> 
      let res = save_gc drawable in
      let color = match combine_existential_status_for_tree existentials with
	| Fully_instantiated -> !proved_complete_gdk_color
	| Partially_instantiated -> !proved_partial_gdk_color
	| Uninstantiated -> !proved_incomplete_gdk_color
      in
      drawable#set_foreground (`COLOR color);
      res
    | Cheated -> 
      let res = save_gc drawable in
      drawable#set_foreground (`COLOR !cheated_gdk_color);
      res



(*****************************************************************************)
(*****************************************************************************)
(** {3 Double linked trees} *)
(*****************************************************************************)
(*****************************************************************************)

(** The proof trees in the proof-tree display are organized as
    doubly-linked trees, where children contain a link to their parent
    nodes. This is needed, because, for efficiency, the tree layout
    computation starts at the last inserted child and walks upwards to
    the root of the tree.
*)

(** Abstract base class for doubly linked trees. Because of
    type-checking problems the functionality for setting and clearing
    children nodes is not inside the class but outside, in the
    functions {!Draw_tree.set_children} and
    {!Draw_tree.clear_children}.
*)
class virtual ['a] doubly_linked_tree =
object 
  (** The parent link. *)
  val mutable parent = None
  
  (** The childrens list. *)
  val mutable children = []


  (** Accessor method for the parent field. *)
  method parent = parent

  (** Low-level setter for the {!parent} field. To insert child nodes
      into the tree, use {!Draw_tree.set_children}. 
  *)
  method set_parent (p : 'a) = parent <- Some p

  (** Another low-level setter for the parent field. To delete nodes
      from the tree, use {!Draw_tree.clear_children} on the parent.
  *)
  method clear_parent = parent <- None

  (** Accessor for the children field. *)
  method children = children

  (** Low-level setter for the children field. To insert child nodes
      into the tree, use {!Draw_tree.set_children}.
  *)
  method set_children (cs : 'a list) = 
    children <- cs

  (** Method to be called when the children list has been changed. *)
  method virtual children_changed : unit
end

(** [set_children parent children] correctly insert [children] into
    the doubly linked tree as children of node [parent]. After the
    change {!doubly_linked_tree.children_changed} is called on
    [parent]. Asserts that the children list of [parent] is empty.
*)
let set_children parent children =
  assert(parent#children = []);
  parent#set_children children;
  List.iter (fun c -> c#set_parent parent) children;
  parent#children_changed


(** [clear_children parent] removes all children from [parent] from
    the doubly linked tree. After the change
    {!doubly_linked_tree.children_changed} is called on [parent].
*)
let clear_children parent =
  List.iter (fun c -> c#clear_parent) parent#children;
  parent#set_children [];
  parent#children_changed

(* 
 * let add_child parent child =
 *   parent#set_children (parent#children @ [child]);
 *   child#set_parent parent;
 *   parent#children_changed
 *)

(* 
 * let remove_child child =
 *   match child#parent with
 *     | None -> ()
 *     | Some p -> 
 *       p#set_children (List.filter (fun c -> c <> child) p#children);
 *       child#clear_parent;
 *       p#children_changed
 *)


(*****************************************************************************)
(*****************************************************************************)
(** {3 Tree layer interface} *)
(*****************************************************************************)
(*****************************************************************************)

(** Abstract interface for {!class: Tree_layers.tree_layer} and
    {!class: Tree_layers.tree_layer_stack}. Root nodes of proof trees and
    layers contain a pointer to the layer or layer stack containing
    them. This pointer is used to invalidate the size information in
    these structures and to query location information. This class
    type breaks the mutual dependency between root nodes and layers
    and layers and the layer stack. The type parameter stands for the
    structure containing the upward pointer, because it passes [self]
    as first argument to {!child_offsets}.
*)
class type ['a] abstract_tree_container =
object

  (** Invalidate the size information in this container and all bigger
      structures containing it.
  *)
  method clear_size_cache : unit

  (** Compute the left and top offset of this container relative to
      the upper-left corner of the complete display. 
  *)
  method left_top_offset : int * int

  (** Compute the x and y offset of one child relative to the upper
      left corner of this container.
  *)
  method child_offsets : 'a -> int * int
end


(*****************************************************************************)
(*****************************************************************************)
(** {3 External window interface} *)
(*****************************************************************************)
(*****************************************************************************)

(** Abstract class type for external {!class: Node_window.node_window}'s
    containing just those methods that are needed here. This class
    type is used to break the circular dependency between {!Draw_tree}
    and {!Node_window}. All {!proof_tree_element}'s keep a list of
    their external windows to update them. External node windows have
    a pointer to proof-tree elements to deregister themselves when
    they get deleted or orphaned. Before external node windows are
    passed to functions in this module, they must be cast to this
    class type.
*)
class type external_node_window =
object
  (** Number of this node window. Used to correlate node windows with
      the proof-tree display.
  *)
  method window_number : string

  (** Update the content in the text buffer of this node window. The
      argument is the updated {!proof_tree_element.sequent_text_history}.
  *)
  method update_content : string list -> unit

  (** Reconfigure and redraw the node window. Needs to be called when
      the configuration has been changed. Actually only the font of
      the buffer text is changed.
  *)
  method configuration_updated : unit

  (** Delete this node window if it is not sticky. Needs to be called
      when the corresponding element in the proof-tree display is
      deleted.
  *)
  method delete_attached_node_window : unit
end


(*****************************************************************************)
(*****************************************************************************)
(** {2 Generic tree element} *)
(*****************************************************************************)
(*****************************************************************************)


(** Abstract base class for turnstiles and proof commands. Contains
    the code for (relativ) layout, (absolute) coordinates, locating
    mouse button clicks, marking branches and the general drawing
    functions.

    Argument undo_state saves the undo state for the current proof.
    It's value is arbitrary for cloned proof trees.
*)
class virtual proof_tree_element drawable
    undo_state debug_name inst_existentials fresh_existentials = 
object (self)
  inherit [proof_tree_element] doubly_linked_tree

  (***************************************************************************)
  (***************************************************************************)
  (** {2 Internal State Fields} *)
  (***************************************************************************)
  (***************************************************************************)

  (** ID for debugging purposes *)
  method debug_name = (debug_name : string)

  (** The kind of this element. *)
  method virtual node_kind : node_kind

  (** The existentials created for this element. Only non-nil when
      this is a proof command.
  *)
  method fresh_existentials = fresh_existentials

  (** The existentials instantiated by this element. Only non-nil when
      this is a proof command. 
  *)
  method inst_existentials : existential_variable list = inst_existentials

  (** Return the state for this sequent. *)
  method undo_state = (undo_state : int)

  (** The {!class: Gtk_ext.better_drawable} into which this element
      draws itself.
  *)
  val drawable = drawable

                                (***************** inside proof_tree_element *)
  (** The width of this node alone in pixels. Set in the initializer
      of the heirs. *)
  val mutable width = 0

  (** The height of this node alone in pixels. Set in the initializer
      of the heirs. *)
  val mutable height = 0

  (** The total width in pixels of the subtree which has this node as
      root. Computed in
      {!Draw_tree.proof_tree_element.update_subtree_size}. *)
  val mutable subtree_width = 0

  (** The x-offset of the left border of the first child. Or, in other
      words, the distance (in pixels) between the left border of the
      subtree which has this node as root and the the left border of
      the subtree which has the first child as root. Always
      non-negative. Zero if this node has no children. Usually zero,
      non-zero only in unusual cases, for instance if the {!width} of
      this node is larger than the total width of all children.
  *)
  val mutable first_child_offset = 0

  (** The x-offset of the centre of this node. In other words the
      distance (in pixels) between the left border of this node's
      subtree and the x-coordinate of this node.
  *)
  val mutable x_offset = 0

                                (***************** inside proof_tree_element *)
  (** The height of this nodes subtree, counted in tree levels. At
      least 1, because this element occupies already some level. 
  *)
  val mutable subtree_levels = 0

  (** The proof state of this node. *)
  val mutable branch_state = Unproven

  (** [true] if this node is selected and displayed in the sequent
      area of the proof-tree window.
  *)
  val mutable selected = false

  (** The list of external node windows. *)
  val mutable external_windows : external_node_window list = []

  (** The set of all existentials for this node. *)
  val mutable existential_variables = fresh_existentials

  (** Upward pointer to the layer containing this proof tree. Must be
      set for root nodes.
  *)
  val mutable tree_layer = 
    (None : proof_tree_element abstract_tree_container option)

  (** This field is really used only inside {!turnstile}. In a
      turnstile element, it holds the list of all previous versions of
      the sequent text without existential information, except for the
      head, with contains the current sequent {b with} existential
      info. For uniform treatment of external node windows, the
      reference is also used for proof commands. There, it holds just
      one element, the proof command with existentials info.

      The existential info is omitted from old versions of the sequent
      text, because this info is incorrect for sequents that get
      updated. The problem is that the exisitentials change already
      with {!Proof_tree.add_new_goal}, which happens long before
      {!Proof_tree.update_sequent}. A fix for this would require a
      protocol change, which is a bit too much for this little
      feature.
  *)
  val mutable sequent_text_history = []


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Accessors / Setters} *)
  (***************************************************************************)
  (***************************************************************************)

                                (***************** inside proof_tree_element *)
  (** Accessor method of {!attribute: width}. *)
  method width = width

  (** Accessor method of {!attribute: height}. *)
  method height = height

  (** Accessor method of {!attribute: subtree_width}. *)
  method subtree_width = subtree_width

  (** Accessor method of {!attribute: subtree_levels}. *)
  method subtree_levels = subtree_levels

  (** Accessor method of {!attribute: x_offset}. *)
  method x_offset = x_offset

  (** Accessor method of {!attribute: branch_state}. *)
  method branch_state = branch_state

  (** Modification method of {!attribute: branch_state}. *)
  method set_branch_state s = branch_state <- s

  (** Accessor method of {!attribute: selected}. *)
  method is_selected = selected

  (** Modification method of {!attribute: selected}. *)
  method selected b = selected <- b


                                (***************** inside proof_tree_element *)
  (** Accessor method of {!attribute: existential_variables}. *)
  method existential_variables = existential_variables

  (** [inherit_existentials exl] sets this nodes {!attribute:
      existential_variables} as union of {!fresh_existentials} and
      [exl].
  *)
  method inherit_existentials existentials =
    existential_variables <- List.rev_append fresh_existentials existentials

  (** The original text content associated with this element. For
      turnstiles this is the sequent text and for proof commands this is the
      complete proof command.
  *)
  method virtual content : string

  (** [true] if the proof command is abbreviated in the display.
      Always [false] for turnstiles. Used to decide whether to display
      tooltips for proof commands.
  *)
  method virtual content_shortened : bool

  (** Return the sequent ID for turnstiles and the empty string for
      proof commands. For turnstiles the sequent ID is used as
      {!debug_name}. *)
  method virtual id : string

  (** Register the proof tree layer containing this root node. *)
  method register_tree_layer tl =
    assert(tree_layer = None);
    tree_layer <- Some tl

  (** Make {!sequent_text_history} accessible for cloning and for
      reattaching external node windows.
  *)
  method sequent_text_history = sequent_text_history

  (** This method is only used inside {!turnstile} but declared here to
      avoid downcasting during cloning. Set the sequent text history,
      used when cloning. *)
  method set_sequent_text_history history =
    sequent_text_history <- history


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Children Iterators} *)
  (***************************************************************************)
  (***************************************************************************)

                                (***************** inside proof_tree_element *)
  (** General iterator for all children. [iter_children left y a f]
      successively computes the [left] and [y] value of each child and
      calls [f left y c a] for each child [c] (starting with the
      leftmost child) until [f] returns [false]. The [a] value is an
      accumulator. The returned [a] is passed to the invocation of [f]
      for the next child. The last returned [a] is the result of the
      total call of this function.
  *)
  method private iter_children :
    'a . int -> int -> 'a -> 
      (int -> int -> 'a -> proof_tree_element -> ('a * bool)) -> 'a =
    fun left y a f ->
      let left = left + first_child_offset in
      let y = y + !current_config.level_distance in
      let rec doit left a = function
	| [] -> a
	| c::cs -> 
	  let (na, cont) = f left y a c in
	  if cont
	  then doit (left + c#subtree_width) na cs
	  else na
      in
      doit left a children

  (** Unit iterator for all children. Calls [f left y c] for each
      child [c]. *)
  method private iter_all_children_unit left y
    (f : int -> int -> proof_tree_element -> unit) =
    self#iter_children left y ()
      (fun left y () c -> f left y c; ((), true))


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Layout and Size Computation} *)
  (***************************************************************************)
  (***************************************************************************)

                                (***************** inside proof_tree_element *)
  (** Compute the height of the subtree of this element in pixels. *)
  method subtree_height = 
    (subtree_levels - 1) * !current_config.level_distance + 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width

  (** Sets the {!width} and {!height} fields. Called in the
      initializer of the heirs and when the configuration has been
      updated.
  *)
  method private virtual set_node_size : unit

  (** (Re-)compute all (relative) layout information for this node.
      Computes and sets {!attribute: subtree_levels}, {!attribute:
      subtree_width}, {!attribute: x_offset} and
      {!first_child_offset}. *)
  method private update_subtree_size =
    let (children_width, max_levels, last_child) = 
      List.fold_left 
	(fun (sum_width, max_levels, _last_child) c -> 
	  (* 
           * (if parent = None || (match parent with Some p -> p#parent = None)
	   *  then Printf.fprintf (debugc())
	   *     "USS child width %d\n%!" c#subtree_width);
           *)
	  (sum_width + c#subtree_width,
	   (if c#subtree_levels > max_levels 
	    then c#subtree_levels 
	    else max_levels),
	   Some c))
	(0, 0, None)
	children 
    in
                                (***************** inside proof_tree_element *)
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
     * Printf.fprintf (debugc()) 
     *   "USS %s childrens width %d first x_offset %d\n%!"
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
                                (***************** inside proof_tree_element *)
    (* The real condition for the next if is
     *   subtree_width - x_offset < width / 2
     * but it has rounding issues when width is odd.
     *)
    if 2 * (subtree_width - x_offset) < width
    then begin
      (* Part of this node is right of rightmost child.
       * Need to increase subtree_width about the outside part, 
       * which is   width / 2 - (subtree_width - x_offset).
       * Now 
       *    subtree_width + width / 2 - (subtree_width - x_offset) =
       *      x_offset + width / 2
       *)
      subtree_width <- x_offset + (width + 1) / 2;
    end else begin
      (* This node's right side is left of right margin of last child.
       * Nothing to do.
       *)
    end;
    (* 
     * Printf.fprintf (debugc()) 
     *   "USS %s END subtree width %d x_offset %d \
     *    first_child_offset %d height %d\n%!"
     *   self#debug_name
     *   subtree_width
     *   x_offset
     *   first_child_offset
     *   subtree_levels;
     *)
	
                                (***************** inside proof_tree_element *)
  (** Do {!update_subtree_size} in this element and all parent
      elements up to the root of the tree.
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
      | None -> 
	(match tree_layer with
	  | None -> 
	    (* during bottom-up clone copy there is no parent and the
	     * tree_layer will be installed later
	     *)
	    ()
	  | Some sco -> sco#clear_size_cache
	)
      | Some p -> p#update_sizes_in_branch


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Coordinates} *)
  (***************************************************************************)
  (***************************************************************************)

                                (***************** inside proof_tree_element *)
  (** Computes the left offset of [child] relative to the bounding box
      of its parent, which must be this node. *)
  method child_offset child =
    self#iter_children 0 0 0 (fun left _y _a oc -> (left, child <> oc))

  (** Computes the pair [(left_off, y_off)]. [left_off] is the offset
      of the left hand side of the bounding box of this node's
      subtree. [y_off] is the offset of the y-coordinate of this node.
      The offsets are relative to the left and top of the layer stack,
      respectively.
  *)
  method left_y_offsets =
    match parent with
      | None -> 
	(match tree_layer with
	  | None -> assert false
	  | Some tl -> 
	    let (tl_left, tl_top) = tl#left_top_offset in
	    let (me_left, me_top) = 
	      tl#child_offsets (self :> proof_tree_element) in
	    (tl_left + me_left, tl_top + me_top + height / 2)
	)
      | Some p ->
	let (parent_left, parent_y) = p#left_y_offsets in
	let y_off = parent_y + !current_config.level_distance in
	let left_off = 
	  parent_left + p#child_offset (self :> proof_tree_element) 
	in
	(left_off, y_off)

                                (***************** inside proof_tree_element *)
  (** Computes the bounding box (that is a 4-tuple [(x_low, x_high,
      y_low, y_high)]) relative to the upper-left corner of the
      complete display. 
  *)
  method bounding_box_offsets =
    let (left, y) = self#left_y_offsets in
    let x = self#get_x_coordinate left in
    (* 
     * Printf.fprintf (debugc())
     *   "BBO %s\n%!"
     *   self#debug_name;
     *)
    (* 
     * Printf.fprintf (debugc())
     *   "BBO left %d width %d height %d | x %d-%d y %d-%d\n%!"
     *   left width height 
     *   left (left + width) (y - height / 2) (y + height / 2);
     *)
    (x - width / 2, x + width / 2, y - height / 2, y + height / 2)

  (** [bounding_box left top] computes the bounding box (that is a
      4-tuple [(x_low, x_high, y_low, y_high)]) of this node in
      absolute values as floats. Arguments [left] and [top] specify
      the upper left corner of the root node of the proof tree. 
  *)
  method bounding_box left top =
    let (x_l, x_u, y_l, y_u) = self#bounding_box_offsets in
    (float_of_int (x_l + left), 
     float_of_int (x_u + left), 
     float_of_int (y_l + top), 
     float_of_int (y_u + top))


                                (***************** inside proof_tree_element *)
  (** Computes the x-coordinate of this node. Argument [left] must be
      the x-coordinate of the left side of the bounding box of this
      node's subtree.
  *)
  method get_x_coordinate left = left + x_offset


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Drawing} *)
  (***************************************************************************)
  (***************************************************************************)

  (** Draw just this element (without connecting lines) at the
      indicated position. First argument [left] is the left border,
      second argument [y] is the y-coordinate. 
  *)
  method private virtual draw : int -> int -> unit

  (** [line_offset inverse_slope] computes the start offset (as
      [(x_off, y_off)]) for drawing a line that start or ends in this
      node with inverse slope [inverse_slope]. These offsets are
      needed to avoid overdrawing elements with connecting lines. The
      parameter is the inverse slope, because it is always defined,
      because we never draw horizontal lines. Vertical lines do
      appear, for them the real slope is infinite. 
  *)
  method virtual line_offset : float -> (int * int)


                                (***************** inside proof_tree_element *)
  (** Draw the lines from this node to all its children. 

      @param left x-coordinate of the left side of the bounding box of
      this node's subtree
      @param y y-coordinate of this node
  *)
  method private draw_lines left y =
    let x = self#get_x_coordinate left in
    self#iter_all_children_unit left y
      (fun left cy child ->
	let cx = child#get_x_coordinate left in
	let slope = float_of_int(cx - x) /. float_of_int(cy - y) in
	let (d_x, d_y) = self#line_offset slope in
	let (c_d_x, c_d_y) = child#line_offset slope in
	let gc_opt = 
	  save_and_set_gc drawable
	    child#branch_state child#existential_variables 
	in
	drawable#line ~x:(x + d_x) ~y:(y + d_y) 
	  ~x:(cx - c_d_x) ~y:(cy - c_d_y);
	restore_gc drawable gc_opt)


                                (***************** inside proof_tree_element *)
  (** Draw this element's subtree given the left side of the bounding box
      and the y-coordinate of this node. This is the internal draw method 
      that iterates through the tree.

      @param left x-coordinate of the left side of the bounding box of
                  this node's subtree
      @param y y-coordinate of this node
  *)
  method draw_subtree left y =
    (* 
     * Printf.fprintf (debugc())
     * "DST %s parent %s childs %s width %d tree_width %d\n%!"
     *   self#debug_name
     *   (match parent with
     * 	| None -> "None"
     * 	| Some p -> p#debug_name)
     *   (String.concat ", " (List.map (fun c -> c#debug_name) children))
     *   width
     *   subtree_width;
     *)
    let gc_opt = save_and_set_gc drawable branch_state existential_variables in
    self#draw left y;
    restore_gc drawable gc_opt;
    self#draw_lines left y;
    self#iter_all_children_unit left y
      (fun left y child -> child#draw_subtree left y)


                                (***************** inside proof_tree_element *)
  (** Draw this node's subtree given the left and top side of the
      bounding box. This is the external draw method that is called 
      from the outside for the root of the tree.

      @param left x-coordinate of the left side of the bounding box of
                  this node's subtree
      @param top y-coordinate of the top side of the bounding box of this 
                 node's subtree
  *)
  method draw_tree_root left top =
    self#draw_subtree left (top + height / 2)


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Locate Mouse Button Clicks} *)
  (***************************************************************************)
  (***************************************************************************)

                                (***************** inside proof_tree_element *)
  (** Iterate over the proof tree to determine the node that contains
      the point [(bx, by)]. Returns [None] if there is no node that
      contains this point. (If [bx] and [by] are the coordinates of a
      mouse click, then this method returns the node that was
      clicked.)

      @param left x-coordinate of the left side of the bounding box of
                  this node's subtree
      @param y y-coordinate of this node
      @param bx x-coordinate of point
      @param by y-coordinate of point
  *)
  method find_node_for_point left y bx by =
    let top = y - height / 2 in
    if bx >= left && bx <= left + subtree_width &&
      by >= top && by <= top + self#subtree_height
    then
      let x = self#get_x_coordinate left in
      if bx >= x - width/2 && bx <= x + width/2 &&
	by >= y - height/2 && by <= y + height/2
      then
	Some (self :> proof_tree_element)
      else
	self#iter_children left y None
	  (fun left y _a child ->
	    let cres = child#find_node_for_point left y bx by in
	    (cres, cres = None))
    else
      None


                                (***************** inside proof_tree_element *)
  (** Iterate over the proof tree to determine the node that contains
      the point [(bx, by)]. Returns [None] if there is no node that
      contains this point. This is the external version that is called
      from the outside to determine nodes for mouse clicks.

      @param left x-coordinate of the left side of the bounding box of
                 this node's subtree
      @param top y-coordinate of the top side of the bounding box of 
                 this node's subtree
      @param bx x-coordinate of point
      @param by y-coordinate of point
  *)
  method find_node_for_point_root left top bx by =
    self#find_node_for_point left (top + height/2) bx by


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Mark Branches and Nodes} *)
  (***************************************************************************)
  (***************************************************************************)

  (** Apply (the marking function) [f] on this node and all parent
      nodes until [f] returns [false] or the root is reached. 
  *)
  method mark_branch (f : proof_tree_element -> bool) =
    if f (self :> proof_tree_element) then
      match parent with
	| Some p -> p#mark_branch f
	| None -> ()

                                (***************** inside proof_tree_element *)
  (** Mark this element as [CurrentNode] and all the parent nodes as
      [Current] branch, see {!branch_state_type}. Relies on the
      invariant that the parent of a [Current] element is also marked
      [Current].
  *)
  method mark_current =
    self#mark_branch 
      (fun (self : proof_tree_element) -> 
	if self#branch_state = Current 
	then false
	else
	  (self#set_branch_state Current; true));
    branch_state <- CurrentNode

  (** Mark this element as [Proven] and mark all parents [Proven]
      until one parent has an unproven child, see
      {!branch_state_type}.
  *)
  method mark_proved =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	if (List.for_all (fun c -> c#branch_state = Proven) self#children)
	then (self#set_branch_state Proven; 
	      (* 
               * Printf.fprintf (debugc()) 
	       * 	"Mark %s proven\n%!" self#debug_name;
               *)
	      true)
	else false
      )

                                (***************** inside proof_tree_element *)
  (** Mark this node as [Cheated] and mark all parents that have only
      [Cheated] children as [Cheated] as well, see
      {!branch_state_type}.
  *)
  method mark_cheated =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	if (List.for_all (fun c -> c#branch_state = Cheated) self#children)
	then (self#set_branch_state Cheated; true)
	else false
      )

  (** Remove the [Current] and [CurrentNode] marking for the current
      branch up to the root and set the marking of these nodes to
      [Unproven], see {!branch_state_type}.
  *)
  method unmark_current =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	match self#branch_state with
	  | CurrentNode
	  | Current -> 
	    self#set_branch_state Unproven; true
	  | Unproven -> false
	  | Proven
	  | Cheated -> assert false
      )

                                (***************** inside proof_tree_element *)
  (** Remove the [Proved] or [Cheated] mark of this element and all
      parent elements until an [Unproven] or [Current] element is met,
      see {!branch_state_type}. 
  *)
  method unmark_proved_or_cheated =
    self#mark_branch
      (fun (self : proof_tree_element) ->
	match self#branch_state with
	  | Cheated
	  | Proven -> self#set_branch_state Unproven; true
	  | Unproven
	  | CurrentNode
	  | Current -> false
      )

  (** Set all [Current] and [CurrentNode] markings in the subtree of
      this element to [Unproven], see {!branch_state_type}. Used when
      the proof-tree window gets disconnected from the current
      proof.
  *)
  method disconnect_proof =
    (match branch_state with
      | Current
      | CurrentNode -> branch_state <- Unproven
      | Unproven
      | Proven
      | Cheated -> ()
    );
    List.iter (fun c -> c#disconnect_proof) children;


  (***************************************************************************)
  (***************************************************************************)
  (** {2 Misc} *)
  (***************************************************************************)
  (***************************************************************************)

                                (***************** inside proof_tree_element *)
  (** Return the displayed text for proof-tree elements, which
      contains additional information about uninstantiated and
      partially instantiated existentials.
  *)
  method displayed_text =
    let uninst_ex = filter_uninstantiated existential_variables in
    let partial_ex = filter_partially_instantiated existential_variables in
    if uninst_ex = [] && partial_ex = []
    then self#content
    else 
      self#content 
      ^ "\n\n"
      ^ (if uninst_ex <> [] 
	then "Open Existentials: " 
	  ^ (string_of_existential_list uninst_ex)
	else "")
      ^ (if uninst_ex <> [] && partial_ex <> []
	then "; "
	else "")
      ^ (if partial_ex <> []
	then "Partially instantiated: "
	  ^ (string_of_existential_list partial_ex)
	else "")

  (** Register an external window for this element. *)
  method register_external_window win =
    external_windows <- win :: external_windows

  (** Delete an external window from the list of registered external
      windows.
  *)
  method delete_external_window win =
    external_windows <- List.filter (fun w -> w <> win) external_windows

                                (***************** inside proof_tree_element *)
  (** Delete all non-sticky external node windows of this node.
  *)
  method delete_non_sticky_external_windows =
    List.iter (fun w -> w#delete_attached_node_window) external_windows

  (** Propagate this nodes existentials to all its children. This
      method is not recursive. It is used during normal operation,
      where newly added children have themselves no children.
  *)
  method private set_children_existentials =
    List.iter (fun c -> c#inherit_existentials existential_variables)
      children

  (** Propagate existentials recursively down to all children in the
      complete subtree of this element. Necessary after proof-tree
      cloning, because cloning works bottom-up.
  *)
  method propagate_existentials =
    self#set_children_existentials;
    List.iter (fun c -> c#propagate_existentials) children

  (** Update the list of existential variables in displayed sequent
      text and proof commands in the whole subtree of this element.
      This needs to be called when some existential got instantiated
      or when an undo uninstantiates some existential.
  *)
  method update_existentials_info =
    (match sequent_text_history with 
      | [] -> ()
      | _ :: history -> 
	sequent_text_history <- self#displayed_text :: history
    );
    (if external_windows <> [] && existential_variables <> [] 
     then
	List.iter (fun ew -> ew#update_content sequent_text_history)
	  external_windows
    );
    List.iter (fun c -> c#update_existentials_info) children	

                                (***************** inside proof_tree_element *)
  (** Hook to be called when the list of children has been changed.
      Adjusts the relative layout information of this element and all its
      parents and (non-recursively) propagates the existentials to all
      children.
  *)
  method children_changed =
    (* prerr_endline("CHILDS at  " ^ self#debug_name ^ " CHANGED"); *)
    self#update_sizes_in_branch;
    self#set_children_existentials
    (* prerr_endline "END CHILD CHANGED" *)

  (** Adjust layout and size information after the configuration has
      been changed. 
  *)
  method configuration_updated =
    List.iter (fun c -> c#configuration_updated) children;
    List.iter (fun ex -> ex#configuration_updated) external_windows;
    self#set_node_size;
    self#update_subtree_size

end



(*****************************************************************************)
(*****************************************************************************)
(** {3 The tree element for sequents} *)
(*****************************************************************************)
(*****************************************************************************)

(** Specific element class for sequents, which draw themselves as
    turnstile symbols. This class specializes the abstract
    {!proof_tree_element} class for sequent nodes in the proof tree.

    Argument undo_state saves the undo state for the current proof.
    It's value is arbitrary for cloned proof trees.
*)
class turnstile (drawable : better_drawable) 
                undo_state sequent_id sequent_text_option =
object (self)
  inherit proof_tree_element drawable undo_state sequent_id [] [] as super

  (** The pure sequent text. *)
  val mutable sequent_text = 
    match sequent_text_option with
      | Some t -> t
      | None -> "waiting for sequent text"

  (** Pango layout for rendering text. Only created if an ID for
      an external window must be put into the display.
  *)
  val mutable layout = None

  (** This is a [Turnstile] node. *)
  method node_kind = Turnstile

  (** Return the pure sequent text as content. *)
  method content = sequent_text

  (** This method is not relevant for sequent elements. Return always
      false.
  *)
  method content_shortened = false

  (** Make the sequent ID accessible, which is used as debugging name
      for sequent elements.
  *)
  method id = sequent_id

                                         (***************** inside turnstile *)
  (** Update the sequent text to a new version. *)
  method update_sequent new_text =
    (match sequent_text_history with
      | [] -> ()
      | _ :: old -> sequent_text_history <- sequent_text :: old
    );
    sequent_text <- new_text;
    sequent_text_history <- self#displayed_text :: sequent_text_history;
    List.iter (fun ew -> ew#update_content sequent_text_history)
      external_windows

  (** Restore the previous version of the sequent text. *)
  method undo_update_sequent =
    (match sequent_text_history with
      | [] -> assert false
      | _ :: [] ->
	(* This happens when undoing the initial update sequent
	 * command for additional subgoals.
	 *)
	sequent_text_history <- [];
	sequent_text <- "no sequent text available"
      | _ :: old :: rest ->
	sequent_text <- old;
	sequent_text_history <- self#displayed_text :: rest
    );
    List.iter (fun ew -> ew#update_content sequent_text_history)
      external_windows

  (** Return the pango layout object of {!layout}. Create one if there
      is none.
  *)
  method private get_layout =
    match layout with
      | None -> 
	drawable#pango_context#set_font_description !proof_tree_font_desc;
	let l = drawable#pango_context#create_layout
	in
	layout <- Some l;
	l
      | Some l -> l

  (** Update fonts, sizes and layout after the configuration has been
      changed.
  *)
  method configuration_updated =
    layout <- None;
    super#configuration_updated

                                         (***************** inside turnstile *)
  (** Draw the turnstile symbol for this sequent at the indicated
      coordinates.
  *)
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
      ~y;
    (match external_windows with
      | [] -> ()
      | win::_ ->
	let layout = self#get_layout in
	Pango.Layout.set_text layout win#window_number;
	let (w, h) = Pango.Layout.get_pixel_size layout in
	drawable#put_layout 
	  ~x:(x + !current_config.turnstile_number_x_offset - w)
	  ~y:(y - h / 2)
	  layout
    )

                                         (***************** inside turnstile *)
  (** Draw this turnstile node.

      @param left x-coordinate of the left side of the bounding box of
      this node's subtree
      @param y y-coordinate of this node
  *)
  method private draw left y =
    let x = self#get_x_coordinate left in
    (* 
     * Printf.fprintf (debugc()) "DRAW TURN %s l %d t %d x %d y %d\n%!" 
     *   self#debug_name left top x y;
     *)
    self#draw_turnstile x y

  (** Compute the line offsets for sequent nodes, see
      {!proof_tree_element.line_offset}
  *)
  method line_offset slope =
    let radius = !current_config.turnstile_radius + !current_config.line_sep in
    let d_y = sqrt(float_of_int(radius * radius) /. (slope *. slope +. 1.0)) in
    let d_x = slope *. d_y in
    (int_of_float(d_x +. 0.5), int_of_float(d_y +. 0.5))

      
  (** Set width and height of this node. *)
  method private set_node_size =
    width <- 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width +
      !current_config.subtree_sep;
    height <- 
      2 * !current_config.turnstile_radius +
      2 * !current_config.turnstile_line_width

                                         (***************** inside turnstile *)
  initializer
    self#set_node_size;
    (* 
     * Printf.fprintf (debugc()) "INIT %s width %d height %d\n%!"
     *   self#debug_name width height;
     *)
    self#update_subtree_size;
    (match sequent_text_option with
      | None -> ()
      | Some _ -> 
	sequent_text_history <- [self#displayed_text]
    );
    ()

end


(*****************************************************************************)
(*****************************************************************************)
(** {3 The tree element for proof commands} *)
(*****************************************************************************)
(*****************************************************************************)

(** Create a new layout with fonts from the current configuration.
    This function exists, because (I)
    Pango.Layout.set_font_description is missing in Debian Squeeze and
    (II) one cannot call a method in the initializer of the instance
    variable layout.
*)
let make_layout context =
  context#set_font_description !proof_tree_font_desc;
  context#create_layout

(** Specific element class for proof commands. This class specializes
    the generic and abstract {!proof_tree_element} for proof-command
    nodes.

    Argument undo_state saves the undo state for the current proof.
    It's value is arbitrary for cloned proof trees.
*)
class proof_command (drawable_arg : better_drawable) 
  undo_state command debug_name inst_existentials fresh_existentials =
object (self)
  inherit proof_tree_element drawable_arg undo_state debug_name 
    inst_existentials fresh_existentials 
    as super

  (** The part of the proof command that is displayed inside the tree.
      Maybe shorter than {!command}.
  *)
  val mutable displayed_command = ""

  (** The original proof command. If it exceeds the length specified
      in field [proof_command_length] (see {!Configuration.t} and
      {!Configuration.current_config}) then only a part of it is
      displayed in the tree display. *)
  val command = command

  (** Flag to indicate that only a part of the proof command is
      displayed inside the proof-tree display. Used to decide whether to
      display tool-tips for this proof command.
  *)
  val mutable content_shortened = false

  (* XXX Pango.Layout.set_font_description is missing in debian
   * squeeze. Have to use Pango.Context.set_font_description and
   * create new layout objects on every font change.
   *)
  (** The pango layout for rendering the proof command text. *)
  val mutable layout = make_layout drawable_arg#pango_context

  (** Width (in pixels) of the rendered proof command text. *)
  val mutable layout_width = 0

  (** Height (in pixels) of the rendered proof command text. *)
  val mutable layout_height = 0

                              (***************** inside proof_command_length *)
  (** This is a [Proof_command] element, see {!node_kind}. *)
  method node_kind = Proof_command

  (** Return the original complete proof command as content. *)
  method content = command

  (** Return whether the proof command has been shortend in the
      display. Used to decide whether to display tool-tips for this
      proof command.
  *)
  method content_shortened = content_shortened

  (** This method is not relevant for proof commands. Return the empty
      string.
  *)
  method id = ""

  (** Render the proof command in the pango layout. *)
  method private render_proof_command =
    let layout_text = 
      match external_windows with
	| [] -> displayed_command
	| w :: _ ->  w#window_number ^ ": " ^ displayed_command
    in
    Pango.Layout.set_text layout layout_text;
    let (w,h) = Pango.Layout.get_pixel_size layout in
    layout_width <- w;
    layout_height <- h

                              (***************** inside proof_command_length *)
  (** Set {!displayed_command}. Called from the initializer and when
      the configuration has been changed.
  *)
  method private set_displayed_command =
    if Util.utf8_string_length command <= !current_config.proof_command_length
    then begin
      content_shortened <- false;
      displayed_command <- replace_char command '\n' ' '
    end else begin
      content_shortened <- true;
      displayed_command <-
	(replace_char
	   (Util.utf8_string_sub command 
	      (!current_config.proof_command_length - 1))
	   '\n' ' ')
            ^ "\226\128\166" 			(* append horizontal ellipsis *)
    end

  (** Set {!proof_tree_element.width} and {!proof_tree_element.height}
      after rendering the proof command.
  *)
  method private set_node_size =
    self#render_proof_command;
    width <- layout_width + !current_config.subtree_sep;
    height <- layout_height

  (** Update fonts, the displayed command, the size and layout
      information after the configuration has been updated.
  *)
  method configuration_updated =
    self#set_displayed_command;
    layout <- make_layout drawable_arg#pango_context;
    super#configuration_updated

                              (***************** inside proof_command_length *)
  (** Override {!proof_tree_element.register_external_window} because
      the displayed proof command must be rerendered when an external
      window is registered.
  *)
  method register_external_window win =
    super#register_external_window win;
    self#render_proof_command

  (** Override {!proof_tree_element.delete_external_window} because
      the displayed proof command must be rerendered when an external
      window is deleted.
  *)
  method delete_external_window win =
    super#delete_external_window win;
    self#render_proof_command

                              (***************** inside proof_command_length *)
  (** Draw just this command node.

      @param left x-coordinate of the left side of the bounding box of
      this node's subtree
      @param y y-coordinate of this node
  *)
  method private draw left y = 
    let x = self#get_x_coordinate left in
    (* 
     * Printf.fprintf (debugc()) "DRAW TURN %s l %d t %d x %d y %d\n%!" 
     *   self#debug_name left top x y;
     *)
    let crea = List.exists (fun e -> e.existential_mark) fresh_existentials in
    let inst = List.exists (fun e -> e.existential_mark) inst_existentials in
    if crea || inst
    then begin
      let w = layout_width + 1 * !current_config.subtree_sep in
      let h = layout_height + 2 * !current_config.subtree_sep in
      let gc = save_gc drawable in
      if crea 
      then drawable#set_foreground (`COLOR !existential_create_gdk_color)
      else drawable#set_foreground (`COLOR !existential_instantiate_gdk_color);
      drawable#arc ~x:(x - w/2) ~y:(y - h/2) 
	~width:w ~height:h ~filled:true ();
      restore_gc drawable gc
    end;
    drawable#put_layout ~x:(x - layout_width/2) ~y:(y - layout_height/2) layout;
    if selected 
    then
      let w = layout_width + !current_config.turnstile_line_width in
      let h = layout_height + !current_config.turnstile_line_width in
      drawable#rectangle 
	~x:(x - w/2) ~y:(y - h/2) ~width:w ~height:h ();

                              (***************** inside proof_command_length *)
  (** Compute the line offsets for proof-command nodes, see
      {!proof_tree_element.line_offset}
  *)
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
    self#set_displayed_command;
    self#set_node_size;
    (* 
     * Printf.fprintf (debugc()) "INIT %s w %d width %d height %d\n%!"
     *   self#debug_name w width height;
     *)
    self#update_subtree_size;
    sequent_text_history <- [self#displayed_text];
    (* Printf.fprintf (debugc()) "INIT PC %s done\n%!" self#debug_name; *)
    ()

end


(*****************************************************************************)
(*****************************************************************************)
(** {2 Cloning} *)
(*****************************************************************************)
(*****************************************************************************)

(** Helper for {!clone_tree_node} for cloning the existential
    variables. Uses the hash to lookup variables, such that for each
    variable only one {!existential_variable} record is created.
*)
let rec clone_existentials ex_hash ex =
  try Hashtbl.find ex_hash ex.existential_name
  with
    | Not_found -> 
      let deps = List.map (clone_existentials ex_hash) ex.dependencies in
      let nex = { existential_name = ex.existential_name;
		  status = ex.status;
		  existential_mark = false;
		  dependencies = deps;
		}
      in
      Hashtbl.add ex_hash ex.existential_name nex;
      nex

(** Recursively clone all nodes in the given subtree, updating the
    reference for the selected node, if the old selected node is contained
    in the subtree.
 *)
let rec clone_tree_node new_pc new_seq ex_hash 
    old_selected cloned_selected node =
  let cloned_children = 
    List.map 
      (clone_tree_node new_pc new_seq ex_hash old_selected cloned_selected) 
      node#children in
  let clone = match node#node_kind with
    | Proof_command -> 
      (new_pc node#content 
	 (List.map (clone_existentials ex_hash) node#inst_existentials)
	 (List.map (clone_existentials ex_hash) node#fresh_existentials)
	 : proof_command :> proof_tree_element)
    | Turnstile -> 
      let ts = new_seq node#id (Some node#content)
      in
      ts#set_sequent_text_history node#sequent_text_history;
      (ts : turnstile :> proof_tree_element)
  in
  if Some node = old_selected
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

(** Clone a complete proof tree. *)
let clone_proof_tree new_pc new_seq ex_hash old_selected cloned_selected root =
  let cloned_root = 
    clone_tree_node new_pc new_seq ex_hash old_selected cloned_selected root in
  cloned_root#propagate_existentials;
  cloned_root

