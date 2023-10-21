type token_type = 
  | TEXT
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
  | EXCLAMATION_MARK
  | QUESTION_MARK
  | DOLLAR_SIGN
  | ASTERISK
  | AMPERSAND
  | SPACE
  | HYPEN
  | CARET
  | LINE__
  | DASH

type token =
  { text : string;
    position : (int * int);
    _type : token_type;
  }

let token_type_from_string str =
  match str with
  | "(" -> BRACKET_OPEN
  | ")" -> BRACKET_CLOSE
  | "}" -> BRACKET_CURLY_OPEN
  | "{" -> BRACKET_CURLY_CLOSE
  | "," -> COMMA
  | "." -> COLON
  | "\\," -> SEMICOLON
  | "'" -> QUOTE
  | "\"" -> DOUBLEQUOTE
  | "#" -> HASH_SIGN
  | "@" -> AT_SIGN
  | "!" -> EXCLAMATION_MARK
  | "?" -> QUESTION_MARK
  | "$" -> DOLLAR_SIGN
  | "*" -> ASTERISK
  | "&" -> AMPERSAND
  | " " -> SPACE
  | "-" -> HYPEN
  | "^" -> CARET
  | "|" -> LINE__ 
  | "\\-" -> DASH
  | _ -> TEXT

let token_type_from_char ch =
  match ch with
  | '(' -> BRACKET_OPEN
  | ')' -> BRACKET_CLOSE
  | '}' -> BRACKET_CURLY_OPEN
  | '{' -> BRACKET_CURLY_CLOSE
  | ',' -> COMMA
  | '.' -> COLON
  (* | ',' -> SEMICOLON *)
  | '\'' -> QUOTE
  | '"' -> DOUBLEQUOTE
  | '#' -> HASH_SIGN
  | '@' -> AT_SIGN
  | '!' -> EXCLAMATION_MARK
  | '?' -> QUESTION_MARK
  | '$' -> DOLLAR_SIGN
  | '*' -> ASTERISK
  | '&' -> AMPERSAND
  | ' ' -> SPACE
  | '-' -> HYPEN
  | '^' -> CARET
  | '|' -> LINE__
  (* | '-' -> DASH *)
  | _ -> TEXT


let get_token_end_pos template from =
  let initial_token_type = token_type_from_char (String.get template from) in

  let rec aux template from =
    let token_type = token_type_from_char (String.get template from) in

    match token_type with
      | TEXT -> 1 + aux template (from + 1)
      | _ -> 1
  in

  (aux template from) - (if initial_token_type = TEXT then 1 else 0)

let generate_from_string template =

  let rec aux template template_length cursor =
    match cursor with
      | _ when cursor = template_length -> []
      | cursor -> 
        let (pos_start, pos_end) = (cursor, cursor + (get_token_end_pos template cursor) ) in
        { text = (String.sub template pos_start (pos_end - pos_start));
          _type = (token_type_from_string (String.make 1 (String.get template cursor)));
          position = (pos_start, pos_end)
        } :: aux template template_length pos_end
  in

  aux template (String.length template) 0
