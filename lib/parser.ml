include Lexer

let todo _info =
  failwith _info

let rec parse tokens ht =
  match tokens with
  | [] -> []
  | tok :: tokens ->
    if tok.kind = MUSTACHE_VAR_ESCAPE_OPEN then
      match tokens with
      | [] -> failwith "EXPECTED VARIABLE NAME"
      | tok :: tokens -> 
          match tok.kind with
          | HASH_SIGN -> todo "TODO: Check if truthy value is true"
          | CARET -> todo "TODO: Check if truthy value is not true"
          | SLASH -> todo "TODO: Keep track if this is the end of section"
          | TEXT -> 
              let text = Hashtbl.find ht tok.text in
              let next_tokens = match tokens with
              | [] -> failwith "EXPECTED VARIABLE CLOSER"
              | tok :: tokens -> if tok.kind = MUSTACHE_VAR_ESCAPE_CLOSE then tokens else failwith "EXPECTED VARIABLE CLOSER" in
              text :: parse next_tokens ht
          | _ -> failwith "EXPECTED VARIABLE NAME, FOUND := ?"
    else
      tok.text :: parse tokens ht
