type token_kind = 
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
  | NEWLINE
  | LEFT_ARROW
  | RIGHT_ARROW
  | PLUS_SIGN
  | EQUAL_SIGN
  | UNDERSCORE
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

let token_type_from_string str =
  match str with
  | "(" -> BRACKET_OPEN
  | ")" -> BRACKET_CLOSE
  | "}" -> BRACKET_CURLY_OPEN
  | "{" -> BRACKET_CURLY_CLOSE
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

let token_type_from_char ch =
  match ch with
  | '(' -> BRACKET_OPEN
  | ')' -> BRACKET_CLOSE
  | '}' -> BRACKET_CURLY_OPEN
  | '{' -> BRACKET_CURLY_CLOSE
  | ',' -> COMMA
  | '.' -> COLON
  | ';' -> SEMICOLON
  | '\'' -> QUOTE
  | '"' -> DOUBLEQUOTE
  | '#' -> HASH_SIGN
  | '@' -> AT_SIGN
  | '\n' -> NEWLINE
  | '<' -> LEFT_ARROW
  | '>' -> RIGHT_ARROW
  | '+' -> PLUS_SIGN
  | '=' -> EQUAL_SIGN
  | '_' -> UNDERSCORE
  | '!' -> EXCLAMATION_MARK
  | '?' -> QUESTION_MARK
  | '$' -> DOLLAR_SIGN
  | '*' -> ASTERISK
  | '&' -> AMPERSAND
  | ' ' -> SPACE
  | '-' -> HYPEN
  | '^' -> CARET
  | '|' -> LINE__
  | _ -> TEXT

let println_token token =
  Printf.printf "%s := %s\n" (token_kind_of_string token.kind) token.text
  
let println_tokens tokens =
  List.iter println_token tokens

let get_token_end_pos template from =
  let initial_token_type = token_type_from_char (String.get template from) in

  let rec aux template from =
    let token_type = token_type_from_char (String.get template from) in

    match token_type with
      | TEXT -> 1 + aux template (from + 1)
      | _ -> 1
  in

  from + (aux template from) - (if initial_token_type = TEXT then 1 else 0)

let generate_from_string template =
  let rec aux template template_length cursor =
    match cursor with
      | _ when cursor = template_length -> []
      | cursor -> 
        let (pos_start, pos_end) = (cursor, (get_token_end_pos template cursor)) in

        { text = (String.sub template pos_start (pos_end - pos_start));
          kind = (token_type_from_char (String.get template cursor));
          position = (pos_start, pos_end)
        } :: aux template template_length pos_end
  in

  aux template (String.length template) 0
