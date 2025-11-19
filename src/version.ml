(* The current version and helpers for working with version strings *)

(* Current version -- updated for every release.
   The last component of the tuple is the suffix (if any), like "alpha1".
 *)
let version : (int * int * int * string option) = (5, 2, 0, None)

(* Formats a version tuple into a version string. *)
let version_to_string v =
  let major, minor, patch, suffix = v in
  match suffix with
  | Some suffix -> Printf.sprintf "%d.%d.%d-%s" major minor patch suffix
  | None -> Printf.sprintf "%d.%d.%d" major minor patch

(* A shortcut for getting the current version string. *)
let version_string = version_to_string version

(* Parses a version string.

   The goal is to support full semver strings like 4.2.0-beta1 (compliant)
   and also partial versions like 4 (implied 4.0.0) and 4.2 (implied 4.2.0).

   Partial versions are against the semver spec but
   [Plugin.require_version(4)] is much shorter to write and, arguably, easier to read.

   Soupault release versions are always semver-compliant,
   partial version support is just to make life easier for plugin writers.
 *)
let version_of_string vstr =
  (* Scanf is used here because it's easy to use and it's in the standard library.

     That approach can't separate pre-release suffix and build metadata in a case like 1.2.3-beta+20210919.
     However, the semver.org standard says that build metadata must be ignored in comparisons,
     so we don't actually need to do it anyway.
   *)
  try Ok (Scanf.sscanf vstr "%u.%u.%u-%s" (fun v1 v2 v3 v4 -> (v1, v2, v3, Some v4)))
  with _ -> try Ok (Scanf.sscanf vstr "%u.%u.%u" (fun v1 v2 v3 -> (v1, v2, v3, None)))
  with _ -> try Ok (Scanf.sscanf vstr "%u.%u" (fun v1 v2 -> (v1, v2, 0, None)))
  with _ -> try Ok (Scanf.sscanf vstr "%u" (fun v1 -> (v1, 0, 0, None)))
  with _ -> Error (Printf.sprintf {|Could not parse version string "%s"|} vstr)

(* Compares two versions according to semver rules. *)
let compare_versions (l1, l2, l3, _) (r1, r2, r3, _) =
  match compare l1 r1, compare l2 r2, compare l3 r3 with
  | 0, 0, res -> res
  | 0, res, _ -> res
  | res, _, _ -> res

(* Checks if given version string is compatible with the current soupault version,
   according to semver rules.
 *)
let require_version vstr =
  let required_version = version_of_string vstr in
  match required_version with
  | Ok required_version ->
    Ok ((compare_versions version required_version) >= 0)
  | (Error _) as err -> err
