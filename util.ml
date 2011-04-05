
let rec list_last = function
  | [] -> assert false
  | [a] -> a
  | _ :: rest -> list_last rest


let rec search_char buf start stop c =
  if start < stop then
    if buf.[start] = c 
    then Some start
    else search_char buf (start + 1) stop c
  else
    None

let chop_final_newlines s =
  let i = ref (String.length s) in
  while !i > 0 && s.[!i - 1] = '\n' do
    decr i
  done;
  String.sub s 0 !i


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


let error_message_dialog message =
  let err = GWindow.message_dialog ~message
    ~message_type:`ERROR
    ~buttons:GWindow.Buttons.ok () 
  in
  ignore(err#connect#response ~callback:(fun _ -> err#destroy()));
  err#show()

