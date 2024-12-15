open Bytecode
open Gc

let stack : float Stack.t = Stack.create ()
let string_table = Hashtbl.create 10

let escape_sequences =
  [ ("\\n", '\n'); ("\\t", '\t'); ("\\r", '\r'); ("\\\\", '\\') ]

let replace_escape_sequences str =
  let buffer = Buffer.create (String.length str) in
  let rec process_chars i =
    if i >= String.length str then Buffer.contents buffer
    else
      let found_escape =
        List.find_opt
          (fun (esc, _) ->
            String.starts_with ~prefix:esc
              (String.sub str i (String.length str - i)))
          escape_sequences
      in
      match found_escape with
      | Some (esc, char) ->
          Buffer.add_char buffer char;
          process_chars (i + String.length esc)
      | None ->
          Buffer.add_char buffer str.[i];
          process_chars (i + 1)
  in
  process_chars 0

let rec execute_bytecode instructions env pc =
  if pc >= Array.length instructions then
    match Stack.top_opt stack with None -> 0.0 | Some hd -> hd
  else
    match instructions.(pc) with
    | LOAD_INT value ->
        let id = GC.allocate_int value in
        GC.add_root id;
        Stack.push (float_of_int id) stack;
        execute_bytecode instructions env (pc + 1)
    | LOAD_FLOAT value ->
        let id = GC.allocate_float value in
        GC.add_root id;
        Stack.push (float_of_int id) stack;
        execute_bytecode instructions env (pc + 1)
    | LOAD_STRING value ->
        let id = GC.allocate_string value in
        GC.add_root id;
        Stack.push (float_of_int id) stack;
        execute_bytecode instructions env (pc + 1)
    | FADD ->
        if Stack.length stack < 2 then
          failwith "Runtime Error: Stack underflow during FADD"
        else
          let a = Stack.pop stack in
          let b = Stack.pop stack in
          Stack.push (b +. a) stack;
          execute_bytecode instructions env (pc + 1)
    | LOAD_VAR name -> (
        match List.assoc_opt name env with
        | Some (value, _) ->
            Stack.push value stack;
            execute_bytecode instructions env (pc + 1)
        | None -> failwith ("Runtime Error: Variable " ^ name ^ " not found"))
    | STORE_VAR name ->
        if Stack.is_empty stack then
          failwith "Runtime Error: Stack is empty when trying to store variable"
        else if List.mem_assoc name env then
          failwith ("Runtime Error: Variable " ^ name ^ " is already declared")
        else
          let value = Stack.pop stack in
          let env = (name, (value, true)) :: env in
          execute_bytecode instructions env (pc + 1)
    | PRINT ->
        if Stack.is_empty stack then
          failwith "Runtime Error: Stack underflow during PRINT"
        else
          let value = Stack.pop stack in
          let id = int_of_float value in
          (match GC.find_object id with
          | Some (GC.StringObj str) ->
              let proc_str = replace_escape_sequences str in
              Printf.printf "%s" proc_str
          | Some (GC.IntObj i) -> Printf.printf "%Ld" i
          | Some (GC.FloatObj f) -> Printf.printf "%f" f
          | None -> Printf.printf "<Unknown Object>\n");
          execute_bytecode instructions env (pc + 1)

let run instructions =
  try
    GC.run_gc ();
    execute_bytecode (Array.of_list instructions) [] 0
  with Failure msg -> failwith msg
