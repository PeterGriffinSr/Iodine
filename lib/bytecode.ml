open Ast

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

let pp_opcode fmt = function
  | LOAD_INT value -> Format.fprintf fmt "LOAD_INT %d" (Int64.to_int value)
  | LOAD_FLOAT value -> Format.fprintf fmt "LOAD_FLOAT %f" value
  | LOAD_VAR name -> Format.fprintf fmt "LOAD_VAR %s" name
  | STORE_VAR name -> Format.fprintf fmt "STORE_VAR %s" name
  | LOAD_STRING value -> Format.fprintf fmt "LOAD_STRING %s" value
  | FADD -> Format.fprintf fmt "FADD"
  | FSUB -> Format.fprintf fmt "FSUB"
  | FMUL -> Format.fprintf fmt "FMUL"
  | FDIV -> Format.fprintf fmt "FDIV"
  | PRINT -> Format.fprintf fmt "PRINT"
  | POWER -> Format.fprintf fmt "POWER"
  | CONCAT -> Format.fprintf fmt "CONCAT"
  | AND -> Format.fprintf fmt "AND"
  | OR -> Format.fprintf fmt "OR"
  | GREATER -> Format.fprintf fmt "GREATER"
  | LESS -> Format.fprintf fmt "LESS"
  | EQUAL -> Format.fprintf fmt "EQUAL"
  | NEQ -> Format.fprintf fmt "NEQ"
  | GEQ -> Format.fprintf fmt "GEQ"
  | LEQ -> Format.fprintf fmt "LEQ"
  | TOSTRING -> Format.fprintf fmt "TOSTRING"
  | TOINT -> Format.fprintf fmt "TOINT"
  | NOT -> Format.fprintf fmt "NOT"
  | INC -> Format.fprintf fmt "INC"
  | DEC -> Format.fprintf fmt "DEC"
  | JUMP_IF_FALSE label -> Format.fprintf fmt "JUMP_IF_FALSE %d" label
  | JUMP label -> Format.fprintf fmt "JUMP %d" label

let rec compile_expr = function
  | Expr.IntExpr { value } -> [ LOAD_INT value ]
  | Expr.FloatExpr { value } -> [ LOAD_FLOAT value ]
  | Expr.StringExpr { value } -> [ LOAD_STRING value ]
  | Ast.Expr.BinaryExpr { left; operator; right } -> (
      compile_expr left @ compile_expr right
      @
      match operator with
      | Ast.Plus -> [ FADD ]
      | Ast.Minus -> [ FSUB ]
      | Ast.Star -> [ FMUL ]
      | Ast.Slash -> [ FDIV ]
      | Ast.Power -> [ POWER ]
      | Ast.Carot -> [ CONCAT ]
      | Ast.LogicalAnd -> [ AND ]
      | Ast.LogicalOr -> [ OR ]
      | Ast.Greater -> [ GREATER ]
      | Ast.Less -> [ LESS ]
      | Ast.Eq -> [ EQUAL ]
      | Ast.Neq -> [ NEQ ]
      | Ast.Geq -> [ GEQ ]
      | Ast.Leq -> [ LEQ ]
      | _ -> failwith "Unsupported operator")
  | Ast.Expr.VarExpr name -> [ LOAD_VAR name ]
  | Ast.Expr.CallExpr { callee; arguments } -> (
      match callee with
      | Ast.Expr.VarExpr "print" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ PRINT ]
      | Ast.Expr.VarExpr "int" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ TOINT ]
      | Ast.Expr.VarExpr "str" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ TOSTRING ]
      | _ -> failwith "Not implemented")
  | Ast.Expr.UnaryExpr { operator; operand } -> (
      let operand_bytecode = compile_expr operand in
      match operator with
      | Ast.Not -> operand_bytecode @ [ NOT ]
      | Ast.Inc -> operand_bytecode @ [ INC ]
      | Ast.Dec -> operand_bytecode @ [ DEC ]
      | _ -> failwith "Unsupported unary operator")
  | _ -> failwith "Not implemented"

let rec compile_stmt = function
  | Ast.Stmt.ExprStmt expr -> compile_expr expr
  | Ast.Stmt.BlockStmt { body } ->
      let rec compile_body = function
        | [] -> []
        | [ stmt ] -> compile_stmt stmt
        | stmt :: rest -> compile_stmt stmt @ compile_body rest
      in
      compile_body body
  | Stmt.VarDeclStmt { identifier; assigned_value; global = _; is_mutable = _ }
    ->
      let expr_bytecode =
        match assigned_value with
        | Some expr -> compile_expr expr
        | None -> [ LOAD_INT 0L ]
      in
      expr_bytecode @ [ STORE_VAR identifier ]
  | Ast.Stmt.IfStmt { condition; then_branch; else_branch } ->
      let condition_bytecode = compile_expr condition in
      let then_bytecode = compile_stmt then_branch in
      let else_bytecode =
        match else_branch with Some branch -> compile_stmt branch | None -> []
      in
      let then_jump_label = List.length then_bytecode + 1 in
      let else_jump_label = List.length else_bytecode + 1 in
      condition_bytecode
      @ [ JUMP_IF_FALSE (then_jump_label + 1) ]
      @ then_bytecode @ [ JUMP else_jump_label ] @ else_bytecode
  | _ -> failwith "Not implemented"
