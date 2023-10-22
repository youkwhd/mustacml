include Lexer

let rec parse tokens ht =
  match tokens with
  | [] -> []
  | tok :: tokens ->
      if tok.kind = MUSTACHE_VAR_ESCAPE_OPEN then
        match tokens with
        | [] -> failwith "EXPECTED VARIABLE NAME"
        | tok :: tokens -> 
            match tok.kind with
            | TEXT -> 
                let text = Hashtbl.find ht tok.text in
                let next_tokens = match tokens with
                | [] -> failwith "EXPECTED VARIABLE CLOSER"
                | tok :: tokens -> if tok.kind = MUSTACHE_VAR_ESCAPE_CLOSE then tokens else failwith "EXPECTED VARIABLE CLOSER" in
                text :: parse next_tokens ht
            | _ -> failwith "EXPECTED VARIABLE NAME, FOUND := ?"
      else
        tok.text :: parse tokens ht
