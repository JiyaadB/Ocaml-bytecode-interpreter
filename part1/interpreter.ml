type stack_item =
  | IntItem of int
  | StringItem of string
  | BoolItem of bool
  | NameItem of string
  | ErrorItem
  | UnitItem
  | ClosureItem of string * string list * (string * stack_item) list list

exception Return of stack_item

let to_string_helper item =
  match item with
  | IntItem i -> string_of_int i
  | StringItem s -> s
  | BoolItem true -> ":true:"
  | BoolItem false -> ":false:"
  | NameItem n -> n
  | ErrorItem -> ":error:"
  | UnitItem -> ":unit:"
  | ClosureItem _ -> "<closure>"

let read_file_to_list filename =
  let ic = open_in filename in
  let rec read_lines () =
    try
      let line = input_line ic in
      (String.trim line) :: read_lines ()
    with End_of_file ->
      close_in ic;
      []
  in
  read_lines ()

let is_int s =
  try let _ = int_of_string s in true
  with Failure _ -> false

let parse_push_value s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    StringItem (String.sub s 1 (len - 2))
  else if s = ":true:" then BoolItem true
  else if s = ":false:" then BoolItem false
  else if s = ":error:" then ErrorItem
  else if s = ":unit:" then UnitItem
  else if is_int s then IntItem (int_of_string s)
  else
    let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
    let is_digit c = (c >= '0' && c <= '9') in
    if len > 0 && is_alpha s.[0] then
      let rec check_chars i =
        if i = len then true
        else if is_alpha s.[i] || is_digit s.[i] then check_chars (i + 1)
        else false
      in
      if check_chars 1 then NameItem s else ErrorItem
    else ErrorItem

let rec lookup env_list n =
  match env_list with
  | [] -> None
  | (key, value) :: rest -> if key = n then Some value else lookup rest n

let rec lookup_envs envs n =
  match envs with
  | [] -> None
  | env :: rest_envs ->
      (match lookup env n with
       | Some v -> Some v
       | None -> lookup_envs rest_envs n)

let resolve envs item =
  match item with
  | NameItem n ->
      (match lookup_envs envs n with
       | Some v -> v
       | None -> NameItem n)
  | _ -> item

let split_spaces s =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' s)

let rec evaluate_commands command_list stacks envs oc =
  match command_list with
  | [] -> stacks
  | cmd :: remaining_commands ->
      let current_stack, rest_stacks = match stacks with | s :: rs -> (s, rs) | [] -> ([], []) in
      let current_env, rest_envs = match envs with | e :: re -> (e, re) | [] -> ([], []) in

      let update_stack new_stack =
        evaluate_commands remaining_commands (new_stack :: rest_stacks) envs oc
      in

      if cmd = "quit" then stacks

      else if cmd = "let" then
        evaluate_commands remaining_commands ([] :: stacks) ([] :: envs) oc

      else if cmd = "end" then
        let returned_item = match current_stack with
          | top :: _ -> top
          | [] -> ErrorItem
        in
        let outer_stack = match rest_stacks with | s :: _ -> s | [] -> [] in
        let outer_rest_stacks = match rest_stacks with | _ :: rs -> rs | [] -> [] in
        evaluate_commands remaining_commands ((returned_item :: outer_stack) :: outer_rest_stacks) rest_envs oc

      else if String.length cmd >= 4 && String.sub cmd 0 4 = "fun " then
        let parts = split_spaces cmd in
        if List.length parts = 3 then
          let fun_name = List.nth parts 1 in
          let arg_name = List.nth parts 2 in
          
          let rec extract_body cmds depth acc =
            match cmds with
            | [] -> (List.rev acc, [])
            | c :: rest ->
                if String.length c >= 4 && String.sub c 0 4 = "fun " then
                  extract_body rest (depth + 1) (c :: acc)
                else if c = "funEnd" then
                  if depth = 0 then (List.rev acc, rest)
                  else extract_body rest (depth - 1) (c :: acc)
                else
                  extract_body rest depth (c :: acc)
          in
          
          let (body_cmds, rest_commands) = extract_body remaining_commands 0 [] in
          let closure = ClosureItem (arg_name, body_cmds, envs) in
          let new_env = (fun_name, closure) :: current_env in
          evaluate_commands rest_commands ((UnitItem :: current_stack) :: rest_stacks) (new_env :: rest_envs) oc
        else
          update_stack (ErrorItem :: current_stack)

      else if cmd = "call" then
        (match current_stack with
         | arg_item :: fun_item :: rest_stack ->
             let resolved_fun = resolve envs fun_item in
             let is_fun_valid = match resolved_fun with ClosureItem _ -> true | _ -> false in

             let is_arg_valid =
               match arg_item with
               | ErrorItem -> false
               | NameItem n -> (match lookup_envs envs n with Some _ -> true | None -> false)
               | _ -> true
             in

             if not is_fun_valid || not is_arg_valid then
               update_stack (ErrorItem :: current_stack)
             else
               (match resolved_fun with
                | ClosureItem (arg_name, body_cmds, closure_envs) ->
                    let resolved_arg = resolve envs arg_item in
                    let new_envs = [(arg_name, resolved_arg)] :: closure_envs in
                    
                    let return_val =
                      try
                        let final_stacks = evaluate_commands body_cmds [[]] new_envs oc in
                        (match final_stacks with
                         | (top :: _) :: _ -> resolve new_envs top
                         | _ -> ErrorItem)
                      with Return v -> v
                    in
                    evaluate_commands remaining_commands ((return_val :: rest_stack) :: rest_stacks) envs oc
                | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "return" then
        let ret_val = match current_stack with top :: _ -> resolve envs top | [] -> ErrorItem in
        raise (Return ret_val)

      else if cmd = "funEnd" then
        update_stack (ErrorItem :: current_stack)

      else if String.length cmd >= 5 && String.sub cmd 0 5 = "push " then
        let val_str = String.sub cmd 5 (String.length cmd - 5) in
        let item = parse_push_value val_str in
        update_stack (item :: current_stack)

      else if cmd = "pop" then
        (match current_stack with
         | [] -> update_stack (ErrorItem :: current_stack)
         | _ :: rest -> update_stack rest)

      else if cmd = "swap" then
        (match current_stack with
         | i1 :: i2 :: rest -> update_stack (i2 :: i1 :: rest)
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "bind" then
        (match current_stack with
         | v_item :: NameItem n :: rest ->
             let resolved_v = resolve envs v_item in
             (match resolved_v with
              | NameItem _ | ErrorItem -> 
                  update_stack (ErrorItem :: current_stack)
              | _ -> 
                  let new_env = (n, resolved_v) :: current_env in
                  evaluate_commands remaining_commands ((UnitItem :: rest) :: rest_stacks) (new_env :: rest_envs) oc)
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "add" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> update_stack (IntItem (v2 + v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "sub" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> update_stack (IntItem (v2 - v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "mul" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> update_stack (IntItem (v2 * v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "div" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> 
                  if v1 = 0 then update_stack (ErrorItem :: current_stack)
                  else update_stack (IntItem (v2 / v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "rem" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> 
                  if v1 = 0 then update_stack (ErrorItem :: current_stack)
                  else update_stack (IntItem (v2 mod v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "neg" then
        (match current_stack with
         | i1 :: rest ->
             (match resolve envs i1 with
              | IntItem v1 -> update_stack (IntItem (-v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "cat" || cmd = "concat" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | StringItem s1, StringItem s2 -> update_stack (StringItem (s2 ^ s1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "and" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | BoolItem b1, BoolItem b2 -> update_stack (BoolItem (b2 && b1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "or" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | BoolItem b1, BoolItem b2 -> update_stack (BoolItem (b2 || b1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "not" then
        (match current_stack with
         | i1 :: rest ->
             (match resolve envs i1 with
              | BoolItem b1 -> update_stack (BoolItem (not b1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "equal" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> update_stack (BoolItem (v2 = v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "lessThan" then
        (match current_stack with
         | i1 :: i2 :: rest ->
             (match resolve envs i1, resolve envs i2 with
              | IntItem v1, IntItem v2 -> update_stack (BoolItem (v2 < v1) :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "if" then
        (match current_stack with
         | x :: y :: z :: rest ->
             (match resolve envs z with
              | BoolItem true -> update_stack (x :: rest)
              | BoolItem false -> update_stack (y :: rest)
              | _ -> update_stack (ErrorItem :: current_stack))
         | _ -> update_stack (ErrorItem :: current_stack))

      else if cmd = "toString" then
        (match current_stack with
         | top :: rest ->
             let str_val = to_string_helper top in
             update_stack (StringItem str_val :: rest)
         | [] -> update_stack (ErrorItem :: current_stack))

      else if cmd = "println" then
        (match current_stack with
         | top :: rest ->
             (match top with
              | StringItem s ->
                  Printf.fprintf oc "%s\n" s;
                  update_stack rest
              | _ -> update_stack (ErrorItem :: current_stack))
         | [] -> update_stack (ErrorItem :: current_stack))

      else if cmd = "" then 
        update_stack current_stack

      else
        update_stack (ErrorItem :: current_stack)

let interpreter (input_file : string) (output_file : string) : unit =
  let my_commands = read_file_to_list input_file in
  let oc = open_out output_file in
  let _ = evaluate_commands my_commands [ [] ] [ [] ] oc in
  close_out oc