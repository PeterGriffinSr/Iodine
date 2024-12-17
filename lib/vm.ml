open Bytecode
open Gc

let stack : float Stack.t = Stack.create ()
let string_table = Hashtbl.create 10

let rec int_pow base exp =
  if exp < 0L then failwith "Runtime Error: Negative exponent for integer power"
  else if exp = 0L then 1L
  else if exp = 1L then base
  else
    let half = int_pow base (Int64.div exp 2L) in
    let half_squared = Int64.mul half half in
    if Int64.rem exp 2L = 0L then half_squared else Int64.mul half_squared base

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
    | POWER ->
        if Stack.length stack < 2 then
          failwith "Runtime Error: Stack underflow during POWER"
        else
          let a = Stack.pop stack in
          let b = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = int_pow a_val b_val in
              let result_id = GC.allocate_int result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val ** b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val ** b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val ** Int64.to_float b_val in
              let result_id = GC.allocate_float result in
              GC.add_root result_id;
              Stack.push (float_of_int result_id) stack
          | _, _ -> type_error "Invalid types for addition");
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
    | LESS ->
        if Stack.length stack < 2 then
          failwith "Runtime Error: Stack underflow during LESS"
        else
          let a = Stack.pop stack in
          let b = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val < b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val < b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val < b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val < Int64.to_float b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | _, _ -> type_error "Invalid types for less");
          execute_bytecode instructions env (pc + 1)
    | GREATER ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during GREATER"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val > b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val > b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val > b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val > Int64.to_float b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | _, _ -> type_error "Invalid types for GREATER comparison");
          execute_bytecode instructions env (pc + 1)
    | EQUAL ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during EQ"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val = b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val = b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val = b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val = Int64.to_float b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | _, _ -> type_error "Invalid types for EQ comparison");
          execute_bytecode instructions env (pc + 1)
    | NEQ ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during NEQ"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val <> b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val <> b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val <> b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val <> Int64.to_float b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | _, _ -> type_error "Invalid types for NEQ comparison");
          execute_bytecode instructions env (pc + 1)
    | CONCAT ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during CONCAT"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.StringObj a_val), Some (GC.StringObj b_val) ->
              let result = a_val ^ b_val in
              Stack.push (GC.store_string result) stack
          | _, _ -> type_error "Invalid types for CONCAT (strings required)");
          execute_bytecode instructions env (pc + 1)
    | AND ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during AND"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let result = a <> 0.0 && b <> 0.0 in
          Stack.push (if result then 1.0 else 0.0) stack;
          execute_bytecode instructions env (pc + 1)
    | OR ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during OR"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let result = a <> 0.0 || b <> 0.0 in
          Stack.push (if result then 1.0 else 0.0) stack;
          execute_bytecode instructions env (pc + 1)
    | GEQ ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during GEQ"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val >= b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val >= b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val >= b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val >= Int64.to_float b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | _, _ -> type_error "Invalid types for GEQ comparison");
          execute_bytecode instructions env (pc + 1)
    | LEQ ->
        if Stack.length stack < 2 then
          type_error "Runtime Error: Stack underflow during LEQ"
        else
          let b = Stack.pop stack in
          let a = Stack.pop stack in
          let a_type = GC.find_object (int_of_float a) in
          let b_type = GC.find_object (int_of_float b) in
          (match (a_type, b_type) with
          | Some (GC.IntObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val <= b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.FloatObj b_val) ->
              let result = a_val <= b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.IntObj a_val), Some (GC.FloatObj b_val) ->
              let result = Int64.to_float a_val <= b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | Some (GC.FloatObj a_val), Some (GC.IntObj b_val) ->
              let result = a_val <= Int64.to_float b_val in
              Stack.push (if result then 1.0 else 0.0) stack
          | _, _ -> type_error "Invalid types for LEQ comparison");
          execute_bytecode instructions env (pc + 1)
    | NOT ->
        if Stack.is_empty stack then
          type_error "Runtime Error: Stack underflow during NOT"
        else
          let value = Stack.pop stack in
          (match GC.find_object (int_of_float value) with
          | Some (GC.IntObj v) ->
              let result = if v = 0L then 1L else 0L in
              let id = GC.allocate_int result in
              Stack.push (float_of_int id) stack
          | _ -> type_error "Invalid type for NOT (expected integer 0 or 1)");
          execute_bytecode instructions env (pc + 1)
    | INC ->
        if Stack.is_empty stack then
          type_error "Runtime Error: Stack underflow during INC"
        else
          let value = Stack.pop stack in
          (match GC.find_object (int_of_float value) with
          | Some (GC.IntObj v) ->
              let id = GC.allocate_int (Int64.add v 1L) in
              Stack.push (float_of_int id) stack
          | _ -> type_error "Invalid type for INC (expected integer)");
          execute_bytecode instructions env (pc + 1)
    | DEC ->
        if Stack.is_empty stack then
          type_error "Runtime Error: Stack underflow during DEC"
        else
          let value = Stack.pop stack in
          (match GC.find_object (int_of_float value) with
          | Some (GC.IntObj v) ->
              let id = GC.allocate_int (Int64.sub v 1L) in
              Stack.push (float_of_int id) stack
          | _ -> type_error "Invalid type for DEC (expected integer)");
          execute_bytecode instructions env (pc + 1)
    | TOINT ->
        if Stack.is_empty stack then
          type_error "Runtime Error: Stack underflow during TOINT"
        else
          let value = Stack.pop stack in
          (match GC.find_object (int_of_float value) with
          | Some (GC.FloatObj f) ->
              let id = GC.allocate_int (Int64.of_float f) in
              Stack.push (float_of_int id) stack
          | Some (GC.StringObj s) -> (
              try
                let id = GC.allocate_int (Int64.of_string s) in
                Stack.push (float_of_int id) stack
              with Failure _ ->
                type_error "Runtime Error: Invalid string for TOINT")
          | _ -> type_error "Invalid type for TOINT (expected float or string)");
          execute_bytecode instructions env (pc + 1)
    | TOSTRING ->
        if Stack.is_empty stack then
          type_error "Runtime Error: Stack underflow during TOSTRING"
        else
          let value = Stack.pop stack in
          (match GC.find_object (int_of_float value) with
          | Some (GC.IntObj i) ->
              let id = GC.allocate_string (Int64.to_string i) in
              Stack.push (float_of_int id) stack
          | Some (GC.FloatObj f) ->
              let id = GC.allocate_string (string_of_float f) in
              Stack.push (float_of_int id) stack
          | _ ->
              type_error "Invalid type for TOSTRING (expected integer or float)");
          execute_bytecode instructions env (pc + 1)
    | JUMP_IF_FALSE label ->
        let condition = Stack.pop stack in
        if condition = 0.0 then execute_bytecode instructions env (pc + label)
        else execute_bytecode instructions env (pc + 1)
    | JUMP label -> execute_bytecode instructions env (pc + label)

let run instructions =
  try
    GC.run_gc ();
    execute_bytecode (Array.of_list instructions) [] 0
  with
  | Failure msg -> failwith msg
  | TypeError msg -> failwith msg
