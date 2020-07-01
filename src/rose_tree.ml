(* List helper functions *)
module List_utils = struct
  let rec remove p xs =
    match xs with
    | [] -> []
    | y :: ys ->
      if (p y) then ys
      else y :: (remove p ys)

  let rec replace p x xs =
    match xs with
    | [] -> raise Not_found
    | y :: ys ->
      if (p y) then x :: ys
      else y :: (replace p x ys)

  let rec insert_before p x xs =
    match xs with
    | [] -> raise Not_found
    | y :: ys ->
      if (p y) then x :: y :: ys
      else y :: (insert_before p x ys)

  let rec insert_after p x xs =
    match xs with
    | [] -> raise Not_found
    | y :: ys ->
      if (p y) then y :: x :: ys
      else y :: (insert_after p x ys)
end

(* Actual tree *)

(* Extracts a top level section from a flat list of headings.

   Since there can be any number of <h1> elements,
   we have two possible options: consider all headings children of a virtual root,
   or treat it at multiple independent trees.
   The latter approach allows for simpler types, since adding a virtual root
   would require node data to be 'a option _just_ to accomodate the root,
   while all real headings are guaranteed to have non-empty data.
 *)
let take_section get_level hs =
  let rec aux hs section level =
  match hs with
  | [] -> section, []
  | h :: hs ->
    if (get_level h) > level then aux hs (h :: section) level
    else section, (h :: hs)
  in match hs with
  | [] -> failwith "Cannot take any section from an empty list of headings"
  | [h] -> (h, []), []
  | h :: hs ->
    let first_level = get_level h in
    let section, remainder = aux hs [] first_level in
    (h, List.rev section), remainder

module Path_tree = struct

  type ('a, 'b) path_tree = {
    name: 'a;
    data: 'b;
    children: ('a, 'b) path_tree list
  }

  exception Empty_path
  exception Duplicate_child
  exception Nonexistent_path
  exception Insert_error of string

  let make name data = { name = name; data = data; children = [] }

  let make_full name data children = { name = name; data = data; children = children }

  let data_of_node n = n.data
  let children_of_node n = n.children

  let insert_immediate node name data children =
      let new_node = make_full name data children in
      let children' = node.children @ [new_node] in
      { node with children = children' }

  let replace node child =
    let children = node.children in
    let name = child.name in
    let children' = List_utils.replace (fun x -> x.name = name) child children in
    { node with children = children' }

  let find node name =
    List.find_opt (fun x -> x.name = name) node.children

  let rec insert ?(children=[]) node path data =
    match path with
    | [] -> raise Empty_path
    | [name] ->
      (let last_child = find node name in
      match last_child with
      | None -> insert_immediate node name data children
      | (Some _) -> raise Duplicate_child)
    | name :: names ->
      let next_child = find node name in
      match next_child with
      | Some next_child' ->
        let new_node = insert ~children:children next_child' names data in
        replace node new_node
      | None ->
        raise (Insert_error "Path does not exist")

  let from_list get_level tree hs =
    let rec aux tree hs path =
      match hs with
      | [] -> tree
      | _ -> begin
        let ((id, h), children), remainder = take_section get_level hs in
        let new_path = path @ [id] in
        let tree = insert tree new_path h in
        let tree =
          (match children with
          | [] -> tree
          | _ -> aux tree children new_path)
        in begin
          match remainder with
          | [] -> tree
          | _ -> aux tree remainder path
        end
      end
    in
    aux tree hs []  
end

type 'a tree = {
  value: 'a;
  children: ('a tree) list
}

let number_elements xs =
  let rec aux xs num acc =
    match xs with
    | [] -> List.rev acc
    | x :: xs ->
      aux xs (num + 1) ((num, x) :: acc)
  in aux xs 1 []

let break_into_sections get_level hs =
  let rec aux hs acc =
    match hs with
    | [] -> acc
    | _ ->
      let section, remainder = take_section get_level hs in
      aux remainder (section :: acc)
  in List.rev @@ aux hs []

let rec from_path_tree t =
  let data = Path_tree.data_of_node t in
  match (Path_tree.children_of_node t) with
  | [] -> {value=data; children=[]}
  | cs -> {value=data; children=(List.map from_path_tree cs)}

let from_list get_level hs =
  let get_level(_, x) = get_level x in
  let sections = number_elements hs |> break_into_sections get_level in
  List.map (fun ((id, h), cs) -> Path_tree.from_list get_level (Path_tree.make id h) cs) sections |>
  List.map from_path_tree   
