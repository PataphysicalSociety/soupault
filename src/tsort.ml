(* "Isolated" nodes in this context mean nodes with no dependencies *)
let find_isolated_nodes hash =
  let aux id deps acc =
    match deps with
    | [] -> id :: acc
    | _  -> acc
  in Hashtbl.fold aux hash []

(* Removes all nodes with ids from the node list*)
let remove_nodes nodes hash =
  List.iter (Hashtbl.remove hash) nodes

(* Walks through the hash and removes a dependency
   from all nodes that have it in their dependency lists *)
let remove_dependency hash dep =
  let aux dep hash id =
    let deps = Hashtbl.find hash id in
    let deps =
      if List.exists ((=) dep) deps then
        CCList.remove ~eq:(=) ~key:dep deps
      else deps
    in
    begin
      Hashtbl.remove hash id;
      Hashtbl.add hash id deps
    end
  in
  let ids = CCHashtbl.keys_list hash in
  List.iter (aux dep hash) ids

(** The Kahn's algorithm:
    1. Find nodes that have no dependencies ("isolated") and remove them from the graph hash.
       Add them to the initial sorted nodes list and the list of isolated nodes for the
       first sorting pass.
    2. For every isolated node, walk through the remaining nodes and
       remove it from their dependency list.
       Nodes that only depended on it now have empty dependency lists.
    3. Find all nodes with empty dependency lists and append them to the sorted
       nodes list _and_ the list of isolated nodes to use for the next step
    4. Repeat until the list of isolated nodes is empty
    5. If the graph hash is still not empty, it means there is a cycle.
 *)  
let sort nodes =
  let rec sorting_loop deps hash acc =
    match deps with
    | [] -> acc
    | dep :: deps ->
      let () = remove_dependency hash dep in
      let isolated_nodes = find_isolated_nodes hash in
      let () = remove_nodes isolated_nodes hash in
      sorting_loop (List.append deps isolated_nodes) hash (List.append acc isolated_nodes)
  in
  let nodes_hash = CCHashtbl.of_list nodes in
  let base_nodes = find_isolated_nodes nodes_hash in 
  let () = remove_nodes base_nodes nodes_hash in
  let sorted_node_ids = sorting_loop base_nodes nodes_hash [] in
  let sorted_node_ids = List.append base_nodes sorted_node_ids in
  let remaining_ids = CCHashtbl.keys_list nodes_hash in
  match remaining_ids with
  | [] -> Ok sorted_node_ids
  | _ ->
    Error (Printf.sprintf "Circular widget dependency or dependency on an undefined widget: %s"
           (String.concat " " remaining_ids))

