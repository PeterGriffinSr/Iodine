let () =
  let argv = Array.to_list Sys.argv in
  match argv with
  | _ when List.mem "--help" argv ->
      print_endline IodineLib.Help.usage;
      exit 0
  | _ when List.mem "--version" argv ->
      Printf.printf "Iodine version %s\n" (IodineLib.Version.version_string ());
      exit 0
  | _ -> (
      match List.tl argv with
      | filename :: _ -> (
          let channel = open_in filename in
          let lexbuf = Lexing.from_channel channel in
          try
            let ast = IodineLib.Parser.program IodineLib.Lexer.token lexbuf in
            print_endline (IodineLib.Ast.Stmt.show ast)
          with Failure msg ->
            Printf.eprintf "Failure: %s\n" msg;
            exit 1)
      | [] ->
          prerr_endline "Error: no input file provided";
          exit 1)
