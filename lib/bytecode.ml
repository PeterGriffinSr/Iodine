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
  | PRINT

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
      | _ -> failwith "Unsupported operator")
  | Ast.Expr.VarExpr name -> [ LOAD_VAR name ]
  | Ast.Expr.CallExpr { callee; arguments } -> (
      match callee with
      | Ast.Expr.VarExpr "print" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ PRINT ]
      | _ -> failwith "Not implemented")
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
  | _ -> failwith "Not implemented"
