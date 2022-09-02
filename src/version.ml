(* Version comparison *)

(* Parses a version string.
   The goal is to support full semver strings like 4.2.0-beta1
   and also partial versions like 4 and 4.2,
   since [Plugin.require_version(4)] is much shorter to write and, arguably, easier to read.
 *)
let version_of_string vstr =
  (* The scanf approach won't be able to separate pre-release suffix and build metadata
     in a case like 1.2.3-beta+20210919,
     but since the semver.org standard says that build metadata must be ignored in comparisons,
     we don't actually need to do it anyway. *)
  try Ok (Scanf.sscanf vstr "%u.%u.%u-%s" (fun v1 v2 v3 v4 -> (v1, v2, v3, Some v4)))
  with _ -> try Ok (Scanf.sscanf vstr "%u.%u.%u" (fun v1 v2 v3 -> (v1, v2, v3, None)))
  (* These options are provided for convenience, they are not semver-compliant.
     Since there will never be, say, soupault "9.0-beta" (as opposed to "9.0.0-beta"),
     they will not cause any trouble.
   *)
  with _ -> try Ok (Scanf.sscanf vstr "%u.%u" (fun v1 v2 -> (v1, v2, 0, None)))
  with _ -> try Ok (Scanf.sscanf vstr "%u" (fun v1 -> (v1, 0, 0, None)))
  with _ -> Error (Printf.sprintf "Could not parse version string \"%s\"" vstr)

(* Compares two versions according to semver rules. *)
let compare_versions (l1, l2, l3, _) (r1, r2, r3, _) =
  match compare l1 r1, compare l2 r2, compare l3 r3 with
  | 0, 0, res -> res
  | 0, res, _ -> res
  | res, _, _ -> res

(* Checks if given version string is compatible with this soupault version,
   according to semver rules.
 *)
let require_version vstr =
  let current_version = Defaults.version in
  let required_version = version_of_string vstr in
  match required_version with
  | Ok required_version ->
    (compare_versions current_version required_version) >= 0
  | Error msg -> failwith msg

(* Prints a version message. *)
let print_version () =
  Printf.printf "soupault %s\n" Defaults.version_string;
  print_endline "Copyright 2022 Daniil Baturin et al.";
  print_endline "soupault is free software distributed under the MIT license";
  print_endline "Visit https://www.soupault.app/reference-manual for documentation"
