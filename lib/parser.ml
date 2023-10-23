include Lexer

let todo _info =
  failwith _info

let parse tokens ht =
  let rec aux tokens ht stack =
    match tokens with
    | [] ->
      if not (Stack.is_empty stack) then
        failwith "NOT EMPTY"
      else
        []
    | tok :: tokens ->
      if tok.kind = MUSTACHE_VAR_ESCAPE_OPEN then
        match tokens with
        | [] -> failwith "EXPECTED VARIABLE NAME"
        | tok :: tokens -> 
          match tok.kind with
          | HASH_SIGN -> 
            (match tokens with
             | [] -> failwith "EXPECTED VARIABLE NAME"
             | tok :: tokens ->
              match Hashtbl.find_opt ht tok.text with
              (* Boolean value *)
              | Some _ ->
                let next_tokens = match tokens with
                | [] -> failwith "EXPECTED VARIABLE CLOSER"
                | tok :: tokens -> if tok.kind = MUSTACHE_VAR_ESCAPE_CLOSE then tokens else failwith "EXPECTED VARIABLE CLOSER" in
                Stack.push tok.text stack;
                aux next_tokens ht stack
              | None -> failwith "q")
          | CARET -> todo "TODO: Check if truthy value is not true"
          | SLASH -> 
              (match tokens with
               | [] -> failwith "EXPECTED VARIABLE CLOSER"
               | tok :: tokens ->
                   if Stack.pop stack <> tok.text then
                      failwith "EXPECTED SECTION CLOSER () NOT FOUND"
                   else
                      (match tokens with
                      | [] -> failwith "EXPECTED ESCAPE CLOSER"
                      | tok :: tokens ->
                          if tok.kind = MUSTACHE_VAR_ESCAPE_CLOSE then
                            aux tokens ht stack
                          else
                            failwith "EXPECTED VARIABLE CLOSER"))
          | TEXT -> 
            let text = Hashtbl.find ht tok.text in
            let next_tokens = match tokens with
            | [] -> failwith "EXPECTED VARIABLE CLOSER"
            | tok :: tokens -> if tok.kind = MUSTACHE_VAR_ESCAPE_CLOSE then tokens else failwith "EXPECTED VARIABLE CLOSER" in
            text :: aux next_tokens ht stack
          | _ -> failwith "EXPECTED VARIABLE NAME, FOUND := ?"
      else
        tok.text :: aux tokens ht stack
  in

  aux tokens ht (Stack.create ())
