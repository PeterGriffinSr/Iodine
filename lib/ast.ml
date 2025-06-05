module Type = struct
  type t = SymbolType of { value : string } | Any [@@deriving show]
end

module Expr = struct
  type t =
    | IntExpr of { value : int64 }
    | FloatExpr of { value : float }
    | StringExpr of { value : string }
    | ByteExpr of { value : char }
    | BoolExpr of { value : bool }
    | BinaryExpr of { left : t; operator : Token.t; right : t }
    | VarExpr of string
  [@@deriving show]
end

module Stmt = struct
  type parameter = { name : string; param_type : Type.t } [@@deriving show]

  type t =
    | BlockStmt of { body : t list }
    | ExprStmt of Expr.t
    | VarDeclarationStmt of {
        identifier : string;
        assigned_value : Expr.t option;
        expl_type : Type.t;
      }
  [@@deriving show]
end
