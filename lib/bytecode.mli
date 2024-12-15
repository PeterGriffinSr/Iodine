type opcode =
  | LOAD_INT of int64
  | LOAD_FLOAT of float
  | LOAD_VAR of string
  | STORE_VAR of string
  | LOAD_STRING of string
  | FADD
  | PRINT

val pp_opcode : Format.formatter -> opcode -> unit
val compile_expr : Ast.Expr.t -> opcode list
val compile_stmt : Ast.Stmt.t -> opcode list
