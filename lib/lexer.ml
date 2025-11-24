(* Token types *)
type token_type =
  (* Single character tokens *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star

  (* Multi-character tokens *)
  | Bang | BangEqual
  | Equal | EqualEqual
  | Greater | GreaterEqual
  | Less | LessEqual

  (* Literals *)
  | Identifier | String | Number

  (* Keywords *)
  | And | Class | Else | False | Fn | For | If | Nil
  | Or | Print | Return | Super | This | True | Var | While

  | EOF

let string_of_token_type tt =
	match tt with
	| LeftParen -> "LeftParen"
	| RightParen -> "RightParen"
	| LeftBrace -> "LeftBrace"
	| RightBrace -> "RightBrace"
	| LeftBracket -> "LeftBracket"
	| RightBracket -> "RightBracket"
	| Comma -> "Comma"
	| Dot -> "Dot"
	| Minus -> "Minus"
	| Plus -> "Plus"
	| Semicolon -> "Semicolon"
	| Slash -> "Slash"
	| Star -> "Star"
	| Bang -> "Bang"
	| BangEqual -> "BangEqual"
	| Equal -> "Equal"
	| EqualEqual -> "EqualEqual"
	| Greater -> "Greater"
	| GreaterEqual -> "GreaterEqual"
	| Less -> "Less"
	| LessEqual -> "LessEqual"
	| Identifier -> "Identifier"
	| String -> "String"
	| Number -> "Number"
	| And -> "And"
	| Class -> "Class"
	| Else -> "Else"
	| False -> "False"
	| Fn -> "Fn"
	| For -> "For"
	| If -> "If"
	| Nil -> "Nil"
	| Or -> "Or"
	| Print -> "Print"
	| Return -> "Return"
	| Super -> "Super"
	| This -> "This"
	| True -> "True"
	| Var -> "Var"
	| While -> "While"
	| EOF -> "EOF"

(** Represents a lexical token *)
type token = {
  token_type: token_type;
  lexeme: string;
  line: int;
  literal: string option
}

(** Converts a token record to a string representation. *)
let token_to_string ({token_type; lexeme; literal; _} : token) =
	Printf.sprintf "%s %s %s" (string_of_token_type token_type) lexeme (match literal with
		| Some lit -> lit
		| None -> "nil"
	)

(** Scans tokens into a list of tokens. *)
let scan_tokens (source : string) : token list =
	let rec scan pos line tokens =
		if pos >= String.length source then
			List.rev ({ token_type = EOF; lexeme = ""; literal = None; line = 1 } :: tokens)
		else
			let char = source.[pos] in
			match char with
			(* Single character tokens *)
			| '(' -> scan (pos + 1) line ({ token_type=LeftParen; lexeme="("; literal=None; line} :: tokens)
			| ')' -> scan (pos + 1) line ({ token_type=RightParen; lexeme=")"; literal=None; line } :: tokens)
			| '{' -> scan (pos + 1) line ({ token_type=LeftBrace; lexeme="{"; literal=None; line } :: tokens)
			| '}' -> scan (pos + 1) line ({ token_type=RightBrace; lexeme="}"; literal=None; line } :: tokens)
			| '[' -> scan (pos + 1) line ({ token_type=LeftBracket; lexeme="["; literal=None; line } :: tokens)
            | ']' -> scan (pos + 1) line ({ token_type=RightBracket; lexeme="]"; literal=None; line } :: tokens)
			| ',' -> scan (pos + 1) line ({ token_type=Comma; lexeme=","; literal=None; line } :: tokens)
			| '.' -> scan (pos + 1) line ({ token_type=Dot; lexeme="."; literal=None; line } :: tokens)
			| '-' -> scan (pos + 1) line ({ token_type=Minus; lexeme="-"; literal=None; line } :: tokens)
			| '+' -> scan (pos + 1) line ({ token_type=Plus; lexeme="+"; literal=None; line } :: tokens)
			| ';' -> scan (pos + 1) line ({ token_type=Semicolon; lexeme=";"; literal=None; line } :: tokens)

			(* Division or comments *)
			| '/' ->
				if pos + 1 < String.length source && source.[pos + 1] = '/' then
					(* Skip till end of line *)
					let rec skip_comment p =
						if p >= String.length source || source.[p] = '\n' then
							p
						else
							skip_comment (p + 1)
					in let end_pos = skip_comment (pos + 2) in
					scan end_pos line tokens
				else
					scan (pos + 1) line ({ token_type=Slash; lexeme="/"; literal=None; line } :: tokens)

			| '*' -> scan (pos + 1) line ({ token_type=Star; lexeme="*"; literal=None; line } :: tokens)
			| '!' ->
				if pos + 1 < String.length source && source.[pos + 1] = '=' then
					scan (pos + 2) line ({ token_type=BangEqual; lexeme="!="; literal=None; line } :: tokens)
				else
					scan (pos + 1) line ({ token_type=Bang; lexeme="!"; literal=None; line } :: tokens)
			| '=' ->
				if pos + 1 < String.length source && source.[pos + 1] = '=' then
					scan (pos + 2) line ({ token_type=EqualEqual; lexeme="=="; literal=None; line } :: tokens)
				else
					scan (pos + 1) line ({ token_type=Equal; lexeme="="; literal=None; line } :: tokens)
			| '<' ->
				if pos + 1 < String.length source && source.[pos + 1] = '=' then
					scan (pos + 2) line ({ token_type=LessEqual; lexeme="<="; literal=None; line } :: tokens)
				else
					scan (pos + 1) line ({ token_type=Less; lexeme="<"; literal=None; line } :: tokens)
			| '>' ->
				if pos + 1 < String.length source && source.[pos + 1] = '=' then
					scan (pos + 2) line ({ token_type=GreaterEqual; lexeme=">="; literal=None; line } :: tokens)
				else
					scan (pos + 1) line ({ token_type=Greater; lexeme=">"; literal=None; line } :: tokens)
			| ' ' | '\t' | '\r' -> scan (pos + 1) line tokens
			| '\n' -> scan (pos + 1) (line + 1) tokens

			(* String literals *)
			| '"' ->
				(* Returns positional index of closing quote. *)
				let rec scan_string_lit (p : int) =
					if p >= String.length source || source.[p] = '"' then
						p
					else
						scan_string_lit (p + 1)
				in let end_pos = scan_string_lit (pos + 1);
				in let lexeme = String.sub source (pos) (end_pos - pos + 1)
				in scan (end_pos + 1) (line) ({ token_type=String; lexeme=lexeme; literal=Some (String.sub lexeme 1 ((String.length lexeme) - 2)); line } :: tokens);

			(* Numeric literals *)
			| '0' .. '9' ->
				(* Returns the number after the last positional index of the number literal. *)
				let rec scan_num_lit (p : int) (frac: bool) =
					if p >= String.length source then
						p
					else
						match source.[p] with
							| '0' .. '9' -> scan_num_lit (p + 1) (frac)
							| '.' ->
								if frac then
									p
								else scan_num_lit (p + 1) true
							| _ -> p

				in let end_pos = scan_num_lit (pos) false in
				let lexeme = String.sub source pos (end_pos - pos) in

				scan (end_pos) line ({
					token_type=Number;
					lexeme;
					literal=Some lexeme;
					line
				} :: tokens)

			(* Alpha lexer *)
			| 'a' .. 'z' | 'A' .. 'Z' | '_' ->
				(* Returns the index of the first character after the identifier. *)
				let rec scan_identifier (p : int) =
					if p >= String.length source then
						p
					else
						match source.[p] with
						| 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> scan_identifier (p + 1)
						| _ -> p
				in let end_pos = scan_identifier pos in

				let lexeme = String.sub source pos (end_pos - pos) in
				let token_type = match lexeme with
					| "and" -> And
					| "class" -> Class
					| "else" -> Else
					| "false" -> False
					| "for" -> For
					| "fn" -> Fn
					| "if" -> If
					| "nil" -> Nil
					| "or" -> Or
					| "print" -> Print
					| "ret" -> Return
					| "super" -> Super
					| "this" -> This
					| "true" -> True
					| "var" -> Var
					| "while" -> While
					| _ -> Identifier

				in scan end_pos line ({token_type; lexeme; literal=None; line} :: tokens)

			| char ->
				Error.error line (Printf.sprintf "Unexpected character %c" char);
				scan (pos + 1) line tokens

	in scan 0 1 []

(** Converts a list of tokens to string. *)
let rec tokens_to_string (tokens : token list) =
	match tokens with
	| [] -> ""
	| hd :: tl -> Printf.sprintf "[%s], %s" (token_to_string hd) (tokens_to_string tl)
