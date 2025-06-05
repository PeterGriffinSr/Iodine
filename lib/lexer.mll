{
  open Parser

  let line = ref 1
  let column = ref 0

  let get_line () = !line

  let get_column () = !column

  let update_column () =
    incr column

  let update_line () =
    incr line;
    column := 0

  let update_column_with_lexeme lexbuf =
    let lexeme = Lexing.lexeme lexbuf in
    column := !column + String.length lexeme;
    lexeme

  let token_and_update_column t lexbuf =
    let token_length = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
    column := !column + token_length;
    t
}

let Identifier = ['a'-'z' 'A'-'Z']
let Digits = ['0'-'9']+
let Floats = Digits '.' Digits+

rule token = parse
    | [' ' '\t']    { update_column (); token lexbuf }
    | '\n'          { update_line (); token lexbuf }
    | "//"          { read_comment lexbuf }

    | "("           { token_and_update_column LParen lexbuf }
    | ")"           { token_and_update_column RParen lexbuf }
    | "["           { token_and_update_column LBracket lexbuf }
    | "]"           { token_and_update_column RBracket lexbuf }
    | "{"           { token_and_update_column LBrace lexbuf }
    | "}"           { token_and_update_column RBrace lexbuf }

    | "+"           { token_and_update_column Plus lexbuf }
    | "-"           { token_and_update_column Minus lexbuf }
    | "*"           { token_and_update_column Star lexbuf }
    | "/"           { token_and_update_column Slash lexbuf }
    | "="           { token_and_update_column Assignment lexbuf }
    | ","           { token_and_update_column Comma lexbuf }
    | "."           { token_and_update_column Dot lexbuf }
    | "_"           { token_and_update_column Underscore lexbuf }
    | "|"           { token_and_update_column Pipe lexbuf }
    | "<"           { token_and_update_column Less lexbuf }
    | ">"           { token_and_update_column Greater lexbuf }
    | ":"           { token_and_update_column Colon lexbuf }
    | ";"           { token_and_update_column Semi lexbuf }

    | "=="          { column := !column + 2; Equal }
    | "!="          { column := !column + 2; NotEqual }
    | "<="          { column := !column + 2; LessEqual }
    | ">="          { column := !column + 2; GreaterEqual }
    | "||"          { column := !column + 2; LogicalOr }
    | "&&"          { column := !column + 2; LogicalAnd }
    | "=>"          { column := !column + 2; Implies }
    | "->"          { column := !column + 2; MapsTo }

    | "val"         { ignore (update_column_with_lexeme lexbuf); Val }
    | "mod"         { ignore (update_column_with_lexeme lexbuf); Mod }
    | "use"         { ignore (update_column_with_lexeme lexbuf); Use }
    | "type"        { ignore (update_column_with_lexeme lexbuf); Type }
    | "match"       { ignore (update_column_with_lexeme lexbuf); Match }
    | "with"        { ignore (update_column_with_lexeme lexbuf); With }
    | "if"          { ignore (update_column_with_lexeme lexbuf); If }
    | "then"        { ignore (update_column_with_lexeme lexbuf); Then }
    | "else"        { ignore (update_column_with_lexeme lexbuf); Else }
    | "None"        { ignore (update_column_with_lexeme lexbuf); TNone }
    | "Some"        { ignore (update_column_with_lexeme lexbuf); TSome }
    | "Ok"          { ignore (update_column_with_lexeme lexbuf); TOk }
    | "Error"       { ignore (update_column_with_lexeme lexbuf); TError }

    | "int"         { ignore (update_column_with_lexeme lexbuf); IntType }
    | "float"       { ignore (update_column_with_lexeme lexbuf); FloatType }
    | "string"      { ignore (update_column_with_lexeme lexbuf); StringType }
    | "byte"        { ignore (update_column_with_lexeme lexbuf); ByteType }
    | "bool"        { ignore (update_column_with_lexeme lexbuf); BoolType }

    | Identifier    { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; Ident lexeme }
    | Floats        { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; Float (float_of_string lexeme) }
    | Digits        { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; Int (Int64.of_string lexeme) }
    | "\'" [^'\''] '\''   { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; Byte (lexeme.[1]) }
    | '"' [^'"'] '"'      { let lexeme = Lexing.lexeme lexbuf in column := !column + String.length lexeme; String (String.sub lexeme 1 (String.length lexeme - 2)) }
    | eof           { EOF }

and read_comment = parse
    | '\n'          { incr line; column := 0; token lexbuf }
    | _             { read_comment lexbuf }
    | eof           { EOF }