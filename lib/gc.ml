module GC = struct
  type gc_obj = StringObj of string | IntObj of int64 | FloatObj of float

  let heap = Hashtbl.create 100
  let root_set = Hashtbl.create 10
  let id_counter = ref 0
  let add_root id = Hashtbl.replace root_set id true
  let remove_root id = Hashtbl.remove root_set id
  let find_object id = Hashtbl.find_opt heap id

  let new_id () =
    let id = !id_counter in
    incr id_counter;
    id

  let mark visited id =
    if Hashtbl.mem visited id then ()
    else (
      Hashtbl.add visited id true;
      match Hashtbl.find_opt heap id with
      | Some (StringObj _ | IntObj _ | FloatObj _) -> ()
      | None -> ())

  let mark_roots () =
    let visited = Hashtbl.create 100 in
    Hashtbl.iter (fun id _ -> mark visited id) root_set;
    visited

  let sweep visited =
    Hashtbl.iter
      (fun id _ -> if not (Hashtbl.mem visited id) then Hashtbl.remove heap id)
      heap

  let run_gc () =
    let visited = mark_roots () in
    sweep visited

  let allocate_string str =
    let id = new_id () in
    Hashtbl.add heap id (StringObj str);
    id

  let allocate_int value =
    let id = new_id () in
    Hashtbl.add heap id (IntObj value);
    id

  let allocate_float value =
    let id = new_id () in
    Hashtbl.add heap id (FloatObj value);
    id

  let store_string str =
    let id = allocate_string str in
    add_root id;
    float_of_int id
end
