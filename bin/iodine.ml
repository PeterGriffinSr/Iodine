open IodineLib

let print_help () =
  Printf.printf "Usage: iodine [option] ... [-c cmd | file ] ...\n";
  Printf.printf "Options \n";
  Printf.printf "-h     : print this help message and exit (also --help)\n";
  Printf.printf
    "-v     : print the Iodine version number and exit (also --version)\n"

let print_version () = Printf.printf "Iodine %s\n" (Version.version ())

let read_file filename =
  let input_channel = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := input_line input_channel :: !lines
    done;
    []
  with End_of_file ->
    close_in input_channel;
    List.rev !lines

let print_error_line filename line_num column =
  let lines = read_file filename in
  if line_num > 0 && line_num <= List.length lines then (
    let error_line = List.nth lines (line_num - 1) in
    Printf.eprintf "%s\n" error_line;
    Printf.eprintf "%s^\n" (String.make (column - 1) ' '))
  else Printf.eprintf "Error: could not locate the line causing the error.\n"

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
  | Parser.Error ->
      let line = Lexer.get_line () in
      let column = Lexer.get_column () in
      Printf.eprintf "SyntaxError: error at line %d, column %d\n" line column;
      print_error_line !filename line column;
      close_in_noerr input_channel;
      exit 1
  | e ->
      close_in_noerr input_channel;
      raise e
