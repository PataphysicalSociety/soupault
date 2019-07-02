(* Exception-safe list tail, assuming a tail of an empty list is an empty list *)
let safe_tail xs =
  match xs with
  | [] -> []
  | _ :: xs' -> xs'

(* Linking batteries just for this would be an overkill... *)
let default default_value opt =
  match opt with
  | None -> default_value
  | Some value -> value
