{
    open Parser

    let line = ref 1
    let column = ref 1
  
    let get_line () = !line
    let get_column () = !column
  
    let update_column () = incr column
    let update_line () = (incr line; column := 1)
  
    let update_column_with_lexeme lexbuf =
        let lexeme = Lexing.lexeme lexbuf in
        column := !column + String.length lexeme;
        lexeme
  
    let token_and_update_column t lexbuf =
        let token_length = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
        column := !column + token_length;
        t
}

let Identifier = ['a'-'z' 'A'-'Z' '_']*
let Digits = ['0'-'9']+
let Floats = Digits '.' Digits+

rule token = parse
    | [' ' '\t']    { update_column (); token lexbuf }
    | '\n'          { update_line (); token lexbuf }
    | '#'           { read_comment lexbuf }

    | "function"    { ignore (update_column_with_lexeme lexbuf); Function }
    | "local"       { ignore (update_column_with_lexeme lexbuf); Local }
    | "return"      { ignore (update_column_with_lexeme lexbuf); Return }
    | "while"       { ignore (update_column_with_lexeme lexbuf); While }
    | "for"         { ignore (update_column_with_lexeme lexbuf); For }
    | "if"          { ignore (update_column_with_lexeme lexbuf); If }
    | "else"        { ignore (update_column_with_lexeme lexbuf); Else }
    | "loop"        { ignore (update_column_with_lexeme lexbuf); Loop }
    | "until"       { ignore (update_column_with_lexeme lexbuf); Until }
    | "break"       { ignore (update_column_with_lexeme lexbuf); Break }
    | "lock"        { ignore (update_column_with_lexeme lexbuf); Lock }

    | "("           { token_and_update_column LParen lexbuf }
    | ")"           { token_and_update_column RParen lexbuf }
    | "{"           { token_and_update_column LBrace lexbuf }
    | "}"           { token_and_update_column RBrace lexbuf }
    | "+"           { token_and_update_column Plus lexbuf }
    | "-"           { token_and_update_column Minus lexbuf }
    | "*"           { token_and_update_column Star lexbuf }
    | "/"           { token_and_update_column Slash lexbuf }
    | ";"           { token_and_update_column Semi lexbuf }
    | ","           { token_and_update_column Comma lexbuf }
    | "!"           { token_and_update_column Not lexbuf }
    | ">"           { token_and_update_column Greater lexbuf }
    | "<"           { token_and_update_column Less lexbuf }
    | "^"           { token_and_update_column Carot lexbuf }
    | "%"           { token_and_update_column Percent lexbuf }
    | "="           { token_and_update_column Assign lexbuf }
    | "&"           { token_and_update_column Ampersand lexbuf }
    | "|"           { token_and_update_column Pipe lexbuf }

    | "**"          { column := !column + 2; Power }
    | "||"          { column := !column + 2; LogicalOr }
    | "&&"          { column := !column + 2; LogicalAnd }
    | "+="          { column := !column + 2; PlusAssign }
    | "-="          { column := !column + 2; MinusAssign }
    | "*="          { column := !column + 2; StarAssign }
    | "/="          { column := !column + 2; SlashAssign }
    | "=="          { column := !column + 2; Eq }
    | "~="          { column := !column + 2; Neq }
    | ">="          { column := !column + 2; Geq }
    | "<="          { column := !column + 2; Leq }
    | "--"          { column := !column + 2; Dec }
    | "++"          { column := !column + 2; Inc }
    | "^^"          { column := !column + 2; Xor }
    | "<<"          { column := !column + 2; Leftshift }
    | ">>"          { column := !column + 2; Rightshift }

    | Identifier            { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; Identifier lexeme }
    | Floats                { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; FloatLit (float_of_string lexeme) }
    | Digits                { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; IntLit (Int64.of_string lexeme) }
    | '\'' [^'\''] '\''     { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; CharLit (lexeme.[1]) }
    | '"' [^'"']* '"'       { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; StringLit (String.sub lexeme 1 (String.length lexeme - 2)) }
    | eof                   { EOF }
    | _                     { let saved_line = get_line () in let saved_column = get_column () in let lexeme = Lexing.lexeme lexbuf in Printf.eprintf "Unexpected token '%s' at Line %d, Column %d\n" lexeme saved_line saved_column; exit (-1) }

and read_comment = parse
    | '\n'                 { incr line; column := 0; token lexbuf }
    | _                    { read_comment lexbuf }
    | eof                  { EOF }