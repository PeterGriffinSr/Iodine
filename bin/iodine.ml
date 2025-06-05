let () =
  let argv = Array.to_list Sys.argv in
  let has_flag flag = List.mem flag argv in
  match List.tl argv with
  | _ when has_flag "--help" -> print_endline IodineLib.Help.usage
  | _ when has_flag "--version" ->
      Printf.printf "Iodine version %s\n" (IodineLib.Version.version_string ())
  | _ :: filename :: _ -> (
      try
        let ast =
          IodineLib.Parser.program IodineLib.Lexer.token
            (Lexing.from_channel (open_in filename))
        in
        print_endline (IodineLib.Ast.Stmt.show ast)
      with Failure msg ->
        Printf.eprintf "Failure: %s\n" msg;
        exit 1)
  | _ ->
      prerr_endline "Error: no input file provided";
      exit 1
