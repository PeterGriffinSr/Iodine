%left LogicalOr
%left LogicalAnd
%nonassoc Equal NotEqual
%nonassoc Less LessEqual Greater GreaterEqual
%left Plus Minus
%left Star Slash

%token Val Mod Use Type Match With If Then Else TNone TSome TOk TError
%token LParen RParen LBracket RBracket LBrace RBrace
%token Plus Minus Star Slash Assignment Comma Dot Underscore Pipe Less Greater Colon Semi
%token Equal NotEqual LessEqual GreaterEqual LogicalOr LogicalAnd Implies MapsTo
%token IntType FloatType StringType ByteType BoolType
%token <string> Ident
%token <int64> Int
%token <string> String
%token <float> Float
%token <char> Byte
%token <bool> Bool
%token EOF

%start program
%type <Ast.Stmt.t> program

%%

program:
    | stmt_list EOF { Ast.Stmt.BlockStmt { body = $1 } }

stmt_list:
    | stmt Semi stmt_list { $1 :: $3 }
    | stmt Semi { [$1] }

stmt:
    | VarDeclStmt { $1 }

type_expr:
    | IntType { Ast.Type.SymbolType { value = "int" } }
    | FloatType { Ast.Type.SymbolType { value = "float" } }
    | StringType { Ast.Type.SymbolType { value = "string" } }
    | ByteType { Ast.Type.SymbolType { value = "byte" } }
    | BoolType { Ast.Type.SymbolType { value = "bool" } }

expr:
    | expr Plus expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Plus; right = $3 } }
    | expr Minus expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Minus; right = $3 } }
    | expr Star expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Star; right = $3 } }
    | expr Slash expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Slash; right = $3 } }
    | expr Greater expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Greater; right = $3 } }
    | expr Less expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Less; right = $3 } }
    | expr LogicalOr expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LogicalOr; right = $3 } }
    | expr LogicalAnd expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LogicalAnd; right = $3 } }
    | expr Equal expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Equal; right = $3 } }
    | expr NotEqual expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.NotEqual; right = $3 } }
    | expr GreaterEqual expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.GreaterEqual; right = $3 } }
    | expr LessEqual expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LessEqual; right = $3 } }
    | Minus Int { Ast.Expr.IntExpr { value = Int64.neg $2 } }
    | Minus Float { Ast.Expr.FloatExpr { value = -. $2 } }
    | Int { Ast.Expr.IntExpr { value = $1 } }
    | Float { Ast.Expr.FloatExpr { value = $1 } }
    | String { Ast.Expr.StringExpr { value = $1 } }
    | Byte { Ast.Expr.ByteExpr { value = $1 } }
    | Bool { Ast.Expr.BoolExpr { value = $1 } }
    | Ident { Ast.Expr.VarExpr $1 }

VarDeclStmt:
    | Val Ident Colon type_expr Assignment expr { Ast.Stmt.VarDeclarationStmt { identifier = $2; assigned_value = Some $6; expl_type = $4; } }