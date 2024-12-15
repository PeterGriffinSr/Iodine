module GC : sig
  type gc_obj = StringObj of string | IntObj of int64 | FloatObj of float

  val heap : (int, gc_obj) Hashtbl.t
  val root_set : (int, bool) Hashtbl.t
  val mark : (int, bool) Hashtbl.t -> int -> unit
  val mark_roots : unit -> (int, bool) Hashtbl.t
  val sweep : (int, 'a) Hashtbl.t -> unit
  val run_gc : unit -> unit
  val allocate_string : string -> int
  val allocate_int : int64 -> int
  val allocate_float : float -> int
  val add_root : int -> unit
  val remove_root : int -> unit
  val find_object : int -> gc_obj option
end
