%left LogicalOr
%left LogicalAnd
%left Pipe
%left Ampersand
%left Eq Neq
%left Geq Leq Greater Less
%left Carot
%left Plus Minus
%left Star Slash Percent
%left Power
%nonassoc Not
%nonassoc Assign PlusAssign MinusAssign StarAssign SlashAssign

%token Function Local Return While For If Else Loop Until Break Lock
%token LParen RParen LBrace RBrace Plus Minus Star Slash Semi Comma Not Greater Less Carot Percent Assign Ampersand Pipe
%token Power LogicalOr LogicalAnd PlusAssign MinusAssign StarAssign SlashAssign Eq Neq Geq Leq Dec Inc Xor Leftshift Rightshift

%token <string> Identifier
%token <int64> IntLit
%token <float> FloatLit
%token <string> StringLit
%token <char> CharLit
%token <bool> BoolLit
%token EOF

%start program
%type <Ast.Stmt.t> program

%%

program:
    | stmt_list EOF { Ast.Stmt.BlockStmt { body = $1 } }

stmt_list:
    | stmt stmt_list { $1 :: $2 }
    | stmt { [$1] }

stmt:
    | simple_stmt { $1 }
    | compound_stmt { $1 }

simple_stmt:
    | VarDeclStmt { $1 }
    | ReturnStmt { $1 }
    | expr { Ast.Stmt.ExprStmt $1 }

parameter_list:
    | parameter Comma parameter_list { $1 :: $3 }
    | parameter { [$1] }
    | { [] }

parameter:
    | Identifier { Ast.Stmt.{ name = $1; } }

argument_list:
    | expr Comma argument_list { $1 :: $3 }
    | expr { [$1] }
    | { [] }

compound_stmt:
    | FuncDeclStmt { $1 }
    | IfStmt { $1 }
    | WhileStmt { $1 }
    | ForStmt { $1 }
    | LoopStmt { $1 }

simple_stmt_opt:
    | simple_stmt { Some $1 }
    | { None }

expr_opt:
    | expr { Some $1 }
    | { None }

expr:
    | expr Plus expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Plus; right = $3 } }
    | expr Minus expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Minus; right = $3 } }
    | expr Star expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Star; right = $3 } }
    | expr Slash expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Slash; right = $3 } }
    | expr Percent expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Percent; right = $3 } }
    | expr Power expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Power; right = $3 } }
    | expr Greater expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Greater; right = $3 } }
    | expr Less expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Less; right = $3 } }
    | expr LogicalOr expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.LogicalOr; right = $3 } }
    | expr LogicalAnd expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.LogicalAnd; right = $3 } }
    | expr Eq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Eq; right = $3 } }
    | expr Neq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Neq; right = $3 } }
    | expr Geq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Geq; right = $3 } }
    | expr Leq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Leq; right = $3 } }
    | expr Carot expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Carot; right = $3 } }
    | expr Ampersand expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Ampersand; right = $3 } }
    | expr Pipe expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Pipe; right = $3 } }
    | expr Xor expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Xor; right = $3 } }
    | expr Leftshift expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Leftshift; right = $3 } }
    | expr Rightshift expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Rightshift; right = $3 } }
    | Identifier LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.VarExpr $1; arguments = $3 } }
    | Not expr { Ast.Expr.UnaryExpr { operator = Ast.Not; operand = $2 } }
    | expr Inc { Ast.Expr.UnaryExpr { operator = Ast.Inc; operand = $1 } }
    | expr Dec { Ast.Expr.UnaryExpr { operator = Ast.Dec; operand = $1 } }
    | Minus IntLit { Ast.Expr.IntExpr { value = Int64.neg $2 } } 
    | Minus FloatLit { Ast.Expr.FloatExpr { value = -. $2 } }
    | Minus Identifier { Ast.Expr.UnaryExpr { operator = Ast.Minus; operand = Ast.Expr.VarExpr $2 } }
    | IntLit { Ast.Expr.IntExpr { value = $1 } }
    | FloatLit { Ast.Expr.FloatExpr { value = $1 } }
    | StringLit { Ast.Expr.StringExpr { value = $1 } }
    | CharLit { Ast.Expr.CharExpr { value = $1 } }
    | BoolLit { Ast.Expr.BoolExpr { value = $1 } }
    | Identifier { Ast.Expr.VarExpr $1 }
    | expr PlusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.PlusAssign; right = $3 } }
    | expr MinusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.MinusAssign; right = $3 } }
    | expr StarAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.StarAssign; right = $3 } }
    | expr SlashAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.SlashAssign; right = $3 } }

VarDeclStmt:
    | Identifier Assign expr { Ast.Stmt.VarDeclStmt { identifier = $1; assigned_value = Some $3; global = true; is_mutable = true; } }
    | Lock Identifier Assign expr { Ast.Stmt.VarDeclStmt { identifier = $2; assigned_value = Some $4; global = true; is_mutable = false; } }
    | Local Identifier Assign expr { Ast.Stmt.VarDeclStmt { identifier = $2; assigned_value = Some $4; global = false; is_mutable = true; } }
    | Lock Local Identifier Assign expr { Ast.Stmt.VarDeclStmt { identifier = $3; assigned_value = Some $5; global = false; is_mutable = false; } }

FuncDeclStmt:
    | Function Identifier LParen parameter_list RParen LBrace stmt_list RBrace { Ast.Stmt.FuncDeclStmt { name = $2; parameters = $4; body = $7; } }

ReturnStmt:
    | Return expr { Ast.Stmt.ReturnStmt $2 }

IfStmt:
    | If LParen expr RParen LBrace stmt_list RBrace Else LBrace stmt_list RBrace { Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = Some (Ast.Stmt.BlockStmt { body = $10 }); } }
    | If LParen expr RParen LBrace stmt_list RBrace { Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = None; } }

WhileStmt:
    | While LParen expr RParen LBrace stmt_list RBrace { Ast.Stmt.WhileStmt { expr = $3; body = $6 } }

ForStmt:
    | For LParen simple_stmt_opt Semi expr_opt Semi simple_stmt_opt RParen LBrace stmt_list RBrace { Ast.Stmt.ForStmt { initialization = $3; condition = (match $5 with Some cond -> cond | None -> Ast.Expr.BoolExpr { value = true }); iteration = $7; body = Ast.Stmt.BlockStmt { body = $10 }; } }

LoopStmt:
    | Loop LBrace stmt_list RBrace Until LParen expr RParen { Ast.Stmt.LoopStmt { condition = $7; body = $3; } }