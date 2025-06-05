type t =
  | Val
  | Mod
  | Use
  | Type
  | Match
  | With
  | If
  | Then
  | Else
  | TNone
  | TSome
  | TOk
  | TError
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Plus
  | Minus
  | Star
  | Slash
  | Assignment
  | Comma
  | Dot
  | Underscore
  | Pipe
  | Less
  | Greater
  | Colon
  | Semi
  | Equal
  | NotEqual
  | LessEqual
  | GreaterEqual
  | LogicalOr
  | LogicalAnd
  | Implies
  | MapsTo
  | IntType
  | FloatType
  | StringType
  | ByteType
  | BoolType
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Byte of char
  | Bool of bool
  | EOF
[@@deriving show]
