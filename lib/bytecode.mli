type opcode =
  | LOAD_INT of int64
  | LOAD_FLOAT of float
  | LOAD_VAR of string
  | STORE_VAR of string
  | LOAD_STRING of string
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | POWER
  | CONCAT
  | AND
  | OR
  | GREATER
  | LESS
  | EQUAL
  | NEQ
  | GEQ
  | LEQ
  | TOSTRING
  | TOINT
  | NOT
  | INC
  | DEC
  | PRINT
  | JUMP_IF_FALSE of int
  | JUMP of int

val pp_opcode : Format.formatter -> opcode -> unit
val compile_expr : Ast.Expr.t -> opcode list
val compile_stmt : Ast.Stmt.t -> opcode list
