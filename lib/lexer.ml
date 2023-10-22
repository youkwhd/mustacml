type token_kind = 
  | TEXT
  | MUSTACHE_VAR_NOESCAPE_OPEN
  | MUSTACHE_VAR_ESCAPE_OPEN
  | MUSTACHE_VAR_NOESCAPE_CLOSE
  | MUSTACHE_VAR_ESCAPE_CLOSE
  | BRACKET_OPEN
  | BRACKET_CLOSE
  | BRACKET_CURLY_OPEN
  | BRACKET_CURLY_CLOSE
  | COMMA
  | COLON
  | SEMICOLON
  | QUOTE
  | DOUBLEQUOTE
  | HASH_SIGN
  | AT_SIGN
  | NEWLINE
  | LEFT_ARROW
  | RIGHT_ARROW
  | PLUS_SIGN
  | EQUAL_SIGN
  | UNDERSCORE
  | SLASH
  | EXCLAMATION_MARK
  | QUESTION_MARK
  | DOLLAR_SIGN
  | ASTERISK
  | AMPERSAND
  | SPACE
  | HYPEN
  | CARET
  | LINE__

let token_kind_of_string token_kind =
  match token_kind with
  | TEXT -> "TEXT"
  | MUSTACHE_VAR_NOESCAPE_OPEN -> "MUSTACHE_VAR_NOESCAPE_OPEN"
  | MUSTACHE_VAR_ESCAPE_OPEN -> "MUSTACHE_VAR_ESCAPE_OPEN"
  | MUSTACHE_VAR_NOESCAPE_CLOSE -> "MUSTACHE_VAR_NOESCAPE_CLOSE"
  | MUSTACHE_VAR_ESCAPE_CLOSE -> "MUSTACHE_VAR_ESCAPE_CLOSE"
  | BRACKET_OPEN -> "BRACKET_OPEN"
  | BRACKET_CLOSE -> "BRACKET_CLOSE"
  | BRACKET_CURLY_OPEN -> "BRACKET_CURLY_OPEN"
  | BRACKET_CURLY_CLOSE -> "BRACKET_CURLY_CLOSE"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | QUOTE -> "QUOTE"
  | DOUBLEQUOTE -> "DOUBLEQUOTE"
  | HASH_SIGN -> "HASH_SIGN"
  | AT_SIGN -> "AT_SIGN"
  | NEWLINE -> "NEWLINE"
  | LEFT_ARROW -> "LEFT_ARROW"
  | RIGHT_ARROW -> "RIGHT_ARROW"
  | PLUS_SIGN -> "PLUS_SIGN"
  | EQUAL_SIGN -> "EQUAL_SIGN"
  | UNDERSCORE -> "UNDERSCORE"
  | SLASH -> "SLASH"
  | EXCLAMATION_MARK -> "EXCLAMATION_MARK"
  | QUESTION_MARK -> "QUESTION_MARK"
  | DOLLAR_SIGN -> "DOLLAR_SIGN"
  | ASTERISK -> "ASTERISK"
  | AMPERSAND -> "AMPERSAND"
  | SPACE -> "SPACE"
  | HYPEN -> "HYPEN"
  | CARET -> "CARET"
  | LINE__ -> "LINE__"

type token =
  { text : string;
    position : (int * int);
    kind : token_kind;
  }

let token_kind_from_string str =
  match str with
  | "(" -> BRACKET_OPEN
  | ")" -> BRACKET_CLOSE
  | "{{{" -> MUSTACHE_VAR_NOESCAPE_OPEN
  | "{{" -> MUSTACHE_VAR_ESCAPE_OPEN
  | "}}}" -> MUSTACHE_VAR_NOESCAPE_CLOSE
  | "}}" -> MUSTACHE_VAR_ESCAPE_CLOSE
  | "{" -> BRACKET_CURLY_OPEN
  | "}" -> BRACKET_CURLY_CLOSE
  | "," -> COMMA
  | "." -> COLON
  | ";" -> SEMICOLON
  | "'" -> QUOTE
  | "\"" -> DOUBLEQUOTE
  | "#" -> HASH_SIGN
  | "\n" -> NEWLINE
  | "@" -> AT_SIGN
  | "<" -> LEFT_ARROW
  | ">" -> RIGHT_ARROW
  | "+" -> PLUS_SIGN
  | "=" -> EQUAL_SIGN
  | "_" -> UNDERSCORE
  | "/" -> SLASH
  | "!" -> EXCLAMATION_MARK
  | "?" -> QUESTION_MARK
  | "$" -> DOLLAR_SIGN
  | "*" -> ASTERISK
  | "&" -> AMPERSAND
  | " " -> SPACE
  | "-" -> HYPEN
  | "^" -> CARET
  | "|" -> LINE__ 
  | _ -> TEXT

let println_token token =
  Printf.printf "%s := %s\n" (token_kind_of_string token.kind) token.text
  
let println_tokens tokens =
  List.iter println_token tokens

let get_token_end_pos template template_length from =
  let rec aux template _to =
    let token_kind = token_kind_from_string (String.sub template from _to) in

    match token_kind with
      | TEXT ->
          if (from + _to) >= template_length then
            1
          else
            let next_token_kind = token_kind_from_string (String.sub template (from + _to) 1) in
            if next_token_kind = TEXT then
              1 + aux template (_to + 1)
            else
              1
      | BRACKET_CURLY_OPEN ->
          if (from + _to) >= template_length then
            1
          else
            let next_token_kind = token_kind_from_string (String.sub template (from + _to) 1) in
            if next_token_kind = BRACKET_CURLY_OPEN then
              1 + aux template (_to + 1)
            else
              1
      | MUSTACHE_VAR_ESCAPE_OPEN ->
          if (from + _to) >= template_length then
            1
          else
            let next_token_kind = token_kind_from_string (String.sub template (from + _to) 1) in
            if next_token_kind = BRACKET_CURLY_OPEN then
              1 + aux template (_to + 1)
            else
              1
      | BRACKET_CURLY_CLOSE ->
          if (from + _to) >= template_length then
            1
          else
            let next_token_kind = token_kind_from_string (String.sub template (from + _to) 1) in
            if next_token_kind = BRACKET_CURLY_CLOSE then
              1 + aux template (_to + 1)
            else
              1
      | MUSTACHE_VAR_ESCAPE_CLOSE ->
          if (from + _to) >= template_length then
            1
          else
            let next_token_kind = token_kind_from_string (String.sub template (from + _to) 1) in
            if next_token_kind = BRACKET_CURLY_CLOSE then
              1 + aux template (_to + 1)
            else
              1
      | _ -> 1
  in

  from + (aux template 1)

let generate_from_string template =
  let rec aux template template_length cursor =
    match cursor with
      | _ when cursor >= template_length -> []
      | cursor -> 
        let (pos_start, pos_end) = (cursor, (get_token_end_pos template template_length cursor)) in
        let text = (String.sub template pos_start (pos_end - pos_start)) in

        { text = text;
          kind = (token_kind_from_string text);
          position = (pos_start, pos_end)
        } :: aux template template_length pos_end
  in

  aux template (String.length template) 0
