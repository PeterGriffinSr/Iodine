val stack : float Stack.t
val string_table : (int, string) Hashtbl.t
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string

val execute_bytecode :
  Bytecode.opcode array -> (string * (float * bool)) list -> int -> float

val run : Bytecode.opcode list -> float
