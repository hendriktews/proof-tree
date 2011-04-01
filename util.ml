
let rec list_last = function
  | [] -> assert false
  | [a] -> a
  | _ :: rest -> list_last rest



class virtual ['a] doubly_linked_tree =
object 
  val mutable parent = None
  val mutable children = []

  method parent = parent
  method set_parent (p : 'a) = parent <- Some p

  method children = children
  method set_children (cs : 'a list) = 
    children <- cs

  method virtual children_changed : unit
end

let set_children parent children =
  parent#set_children children;
  List.iter (fun c -> c#set_parent parent) children;
  parent#children_changed

