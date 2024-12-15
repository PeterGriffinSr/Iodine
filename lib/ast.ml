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

let pp_token fmt = function
  | Function -> Format.fprintf fmt "Function"
  | Global -> Format.fprintf fmt "Global"
  | Local -> Format.fprintf fmt "Local"
  | Return -> Format.fprintf fmt "Return"
  | While -> Format.fprintf fmt "While"
  | For -> Format.fprintf fmt "For"
  | If -> Format.fprintf fmt "If"
  | Else -> Format.fprintf fmt "Else"
  | Loop -> Format.fprintf fmt "Loop"
  | Until -> Format.fprintf fmt "Until"
  | Break -> Format.fprintf fmt "Break"
  | LParen -> Format.fprintf fmt "LParen"
  | RParen -> Format.fprintf fmt "RParen"
  | LBrace -> Format.fprintf fmt "LBrace"
  | RBrace -> Format.fprintf fmt "RBrace"
  | Plus -> Format.fprintf fmt "Plus"
  | Minus -> Format.fprintf fmt "Minus"
  | Star -> Format.fprintf fmt "Star"
  | Slash -> Format.fprintf fmt "Slash"
  | Semi -> Format.fprintf fmt "Semi"
  | Comma -> Format.fprintf fmt "Comma"
  | Not -> Format.fprintf fmt "Not"
  | Greater -> Format.fprintf fmt "Greater"
  | Less -> Format.fprintf fmt "Less"
  | Carot -> Format.fprintf fmt "Carot"
  | Percent -> Format.fprintf fmt "Percent"
  | Assign -> Format.fprintf fmt "Assign"
  | Ampersand -> Format.fprintf fmt "Ampersand"
  | Pipe -> Format.fprintf fmt "Pipe"
  | Power -> Format.fprintf fmt "Power"
  | LogicalOr -> Format.fprintf fmt "LogicalOr"
  | LogicalAnd -> Format.fprintf fmt "LogicalAnd"
  | PlusAssign -> Format.fprintf fmt "PlusAssign"
  | MinusAssign -> Format.fprintf fmt "MinusAssign"
  | StarAssign -> Format.fprintf fmt "StarAssign"
  | SlashAssign -> Format.fprintf fmt "SlashAssign"
  | Eq -> Format.fprintf fmt "Eq"
  | Neq -> Format.fprintf fmt "Neq"
  | Geq -> Format.fprintf fmt "Geq"
  | Leq -> Format.fprintf fmt "Leq"
  | Dec -> Format.fprintf fmt "Dec"
  | Inc -> Format.fprintf fmt "Inc"
  | Xor -> Format.fprintf fmt "Xor"
  | Leftshift -> Format.fprintf fmt "Leftshift"
  | Rightshift -> Format.fprintf fmt "Rightshift"
  | Identifier s -> Format.fprintf fmt "Identifier(%s)" s
  | Int i -> Format.fprintf fmt "Int(%d)" i
  | Float f -> Format.fprintf fmt "Float(%f)" f
  | String s -> Format.fprintf fmt "String(%s)" s
  | Char c -> Format.fprintf fmt "Char(%c)" c
  | Bool b -> Format.fprintf fmt "Bool(%b)" b
  | EOF -> Format.fprintf fmt "EOF"

module Type = struct
  type t = SymbolType of { value : string } [@@deriving show]
end

module Expr = struct
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
  [@@deriving show]
end

module Stmt = struct
  type parameter = { name : string } [@@deriving show]

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
  [@@deriving show]
end
