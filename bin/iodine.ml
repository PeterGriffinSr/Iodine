open IodineLib

let print_help () =
  Printf.printf "Usage: iodine [option] ... [-c cmd | file ] ...\n";
  Printf.printf "Options \n";
  Printf.printf "-h     : print this help message and exit (also --help)\n";
  Printf.printf
    "-v     : print the Iodine version number and exit (also --version)\n"

let print_version () = Printf.printf "Iodine %s\n" (Version.version ())

let () =
  let speclist =
    [
      ( "--help",
        Arg.Unit
          (fun () ->
            print_help ();
            exit 0),
        "Display this help message" );
      ( "-h",
        Arg.Unit
          (fun () ->
            print_help ();
            exit 0),
        "Display this help message" );
      ( "--version",
        Arg.Unit
          (fun () ->
            print_version ();
            exit 0),
        "Display compiler version information" );
      ( "-v",
        Arg.Unit
          (fun () ->
            print_version ();
            exit 0),
        "Display compiler version information" );
    ]
  in

  let filename = ref "" in
  Arg.parse speclist
    (fun arg -> filename := arg)
    "Iodine: a sample program for demonstrating speclist usage";

  if !filename = "" then (
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1);

  let input_channel = open_in !filename in
  let lexbuf = Lexing.from_channel input_channel in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Printf.printf "%s\n" (Ast.Stmt.show ast);

    let bytecode = Bytecode.compile_stmt ast in
    List.iter
      (fun op ->
        Bytecode.pp_opcode Format.str_formatter op;
        let opcode_str = Format.flush_str_formatter () in
        Printf.printf "Generated opcode: %s\n" opcode_str)
      bytecode;
    let _result = Vm.run bytecode in

    close_in input_channel
  with
  | e ->
      close_in_noerr input_channel;
      raise e
