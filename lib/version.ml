let detect_system () =
  match Sys.os_type with
  | "Win32" -> "Windows"
  | "Unix" ->
      let ic = Unix.open_process_in "uname" in
      let uname = try input_line ic with End_of_file -> "Unknown Unix" in
      let _ = Unix.close_process_in ic in
      String.capitalize_ascii uname
  | _ -> "Unknown"

let version = (0, 1, 0)
let codename = "Amber"

let version_string () =
  let major, minor, patch = version in
  Printf.sprintf "%d.%d.%d (%s) on %s" major minor patch codename
    (detect_system ())
