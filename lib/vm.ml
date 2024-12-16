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

exception TypeError of string

let type_error msg = raise (TypeError ("Type Error: " ^ msg))

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
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = Int64.add a_val b_val in
              let result_id = GC.allocate_int result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val +. b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val +. b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val +. Int64.to_float b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | _, _ -> type_error "Invalid types for addition");
          execute_bytecode instructions env (pc + 1)
    | FSUB ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during FSUB"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = Int64.sub a_val b_val in
              let result_id = GC.allocate_int result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val -. b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val -. b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val -. Int64.to_float b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | _, _ -> type_error "Invalid types for subtraction");
          execute_bytecode instructions env (pc + 1)
    | FMUL ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during FMUL"
        else
          let a = Stack.pop stack in
          let b = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = Int64.mul a_val b_val in
              let result_id = GC.allocate_int result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val *. b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val *. b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val *. Int64.to_float b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | _, _ -> type_error "Invalid types for multiplication");
          execute_bytecode instructions env (pc + 1)
    | FDIV ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during FDIV"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              if b_val = 0L then type_error "Division by zero"
              else
                let result = Int64.div a_val b_val in
                let result_id = GC.allocate_int result in
                GC.add_root result_id;
                Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              if b_val = 0.0 then type_error "Division by zero"
              else
                let result = a_val /. b_val in
                let result_id = GC.allocate_float result in
                GC.add_root result_id;
                Stack.push (float_of_int result_id) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              if b_val = 0.0 then type_error "Division by zero"
              else
                let result = Int64.to_float a_val /. b_val in
                let result_id = GC.allocate_float result in
                GC.add_root result_id;
                Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              if b_val = 0L then type_error "Division by zero"
              else
                let result = a_val /. Int64.to_float b_val in
                let result_id = GC.allocate_float result in
                GC.add_root result_id;
                Stack.push (float_of_int result_id) stack
          | _, _ -> type_error "Invalid types for division");
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
              Printf.printf "%s\n" proc_str
          | Some (GC.IntObj i) -> Printf.printf "%Ld\n" i
          | Some (GC.FloatObj f) -> Printf.printf "%.2f\n" f
          | None -> Printf.printf "<Unknown Object>\n");
          execute_bytecode instructions env (pc + 1)

let run instructions =
  try
    GC.run_gc ();
    execute_bytecode (Array.of_list instructions) [] 0
  with
  | Failure msg -> failwith msg
  | TypeError msg -> failwith msg
