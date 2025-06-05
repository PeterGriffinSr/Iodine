module Type : sig
  type t = SymbolType of { value : string } | Any

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Expr : sig
  type t =
    | IntExpr of { value : int64 }
    | FloatExpr of { value : float }
    | StringExpr of { value : string }
    | ByteExpr of { value : char }
    | BoolExpr of { value : bool }
    | BinaryExpr of { left : t; operator : Token.t; right : t }
    | VarExpr of string

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Stmt : sig
  type parameter = { name : string; param_type : Type.t }

  type t =
    | BlockStmt of { body : t list }
    | ExprStmt of Expr.t
    | VarDeclarationStmt of {
        identifier : string;
        assigned_value : Expr.t option;
        expl_type : Type.t;
      }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
