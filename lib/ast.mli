type token =
  | Function
  | Global
  | Local
  | Return
  | While
  | For
  | If
  | Else
  | Loop
  | Until
  | Break
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Plus
  | Minus
  | Star
  | Slash
  | Semi
  | Comma
  | Not
  | Greater
  | Less
  | Carot
  | Percent
  | Assign
  | Ampersand
  | Pipe
  | Power
  | LogicalOr
  | LogicalAnd
  | PlusAssign
  | MinusAssign
  | StarAssign
  | SlashAssign
  | Eq
  | Neq
  | Geq
  | Leq
  | Dec
  | Inc
  | Xor
  | Leftshift
  | Rightshift
  | Identifier of string
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | EOF

val pp_token : Format.formatter -> token -> unit

module Type : sig
  type t = SymbolType of { value : string }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Expr : sig
  type t =
    | IntExpr of { value : int64 }
    | FloatExpr of { value : float }
    | StringExpr of { value : string }
    | CharExpr of { value : char }
    | BoolExpr of { value : bool }
    | VarExpr of string
    | BinaryExpr of { left : t; operator : token; right : t }
    | TernaryExpr of { cond : t; onTrue : t; onFalse : t }
    | UnaryExpr of { operator : token; operand : t }
    | AssignmentExpr of { identifier : string; value : t option }
    | CallExpr of { callee : t; arguments : t list }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Stmt : sig
  type parameter = { name : string }

  val pp_parameter : Format.formatter -> parameter -> unit
  val show_parameter : parameter -> string

  type t =
    | BlockStmt of { body : t list }
    | VarDeclStmt of {
        identifier : string;
        assigned_value : Expr.t option;
        global : bool;
        is_mutable : bool;
      }
    | FuncDeclStmt of {
        name : string;
        parameters : parameter list;
        body : t list;
      }
    | WhileStmt of { expr : Expr.t; body : t list }
    | IfStmt of { condition : Expr.t; then_branch : t; else_branch : t option }
    | ForStmt of {
        initialization : t option;
        condition : Expr.t;
        iteration : t option;
        body : t;
      }
    | LoopStmt of { condition : Expr.t; body : t list }
    | ExprStmt of Expr.t
    | ReturnStmt of Expr.t
    | BreakStmt

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
