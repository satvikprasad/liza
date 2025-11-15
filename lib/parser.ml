let (>>=) o f = Result.bind o f 

type literal =
    | Number of float
    | String of string
    | True
    | False
    | Nil

type binary_op =
    | EqualEqual
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Plus
    | Minus
    | Star
    | Slash

let binary_op_to_lexeme op =
    match op with
    | EqualEqual -> "=="
    | NotEqual -> "!="
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | Slash -> "/"

type unary_op =
    | Bang
    | Minus

type expr =
    | Literal of literal
    | Unary of unary_op * expr
    | Binary of expr * binary_op * expr
    | Grouping of expr

(** Generates a string representation of a given AST. *)
let rec pretty_print_expr (e : expr): string =
    match e with
    | Literal (Number n) ->
        Printf.sprintf "%f" n
    | Literal (String s) ->
        Printf.sprintf "\"%s\"" s
    | Literal True ->
        Printf.sprintf "True"
    | Literal False ->
        Printf.sprintf "False"
    | Literal Nil ->
        Printf.sprintf "Nil"

    | Unary (Bang, e1) ->
        Printf.sprintf "(! %s)" (pretty_print_expr e1)
    | Unary (Minus, e1) ->
        Printf.sprintf "(- %s)" (pretty_print_expr e1)

    | Binary (left_expr, op, right_expr) ->
        Printf.sprintf "(%s %s %s)" (binary_op_to_lexeme op) (pretty_print_expr left_expr) (pretty_print_expr right_expr)

    | Grouping expr ->
        pretty_print_expr expr |> Printf.sprintf "(%s)"

exception GroupException of string
exception InvalidTokenException of string

type eval_error = 
    | TypeError of string * expr

type parser_error = 
    | InvalidToken of string * Lexer.token option
    | MissingSemicolon
    | EvalError of eval_error
    | PrintInvalidExpression
    | ExpectedEOF

(** Parses primary grammar *)
let rec parse_primary (tokens : Lexer.token list): (expr * Lexer.token list, parser_error) result =
    match tokens with
    | { token_type = Lexer.String; literal = Some lit; _ } :: tl -> Ok (Literal (String lit), tl)
    | { token_type = Lexer.Number; literal = Some lit; _ } :: tl -> Ok (Literal (Number (Float.of_string lit)), tl)
    | { token_type = Lexer.True; _ } :: tl -> Ok (Literal True, tl)
    | { token_type = Lexer.False; _ } :: tl -> Ok (Literal False, tl)
    | { token_type = Lexer.Nil; _ } :: tl -> Ok (Literal Nil, tl)

    | { token_type = Lexer.LeftParen; _ } :: tl ->
        parse_expression tl >>= fun (inner_exp, rest) -> (
            match rest with
            | {token_type = Lexer.RightParen; _} :: rest' -> Ok (Grouping inner_exp, rest')
            | tok :: _ -> Error (InvalidToken (
                Printf.sprintf "Expected right paren after left paren grouping, but encountered %s" tok.lexeme, 
                Some tok
            ))
            | [] -> Error (InvalidToken ("Expected right paren after left paren grouping, but ran out of tokens.", None))
        )

    | hd :: _ -> (* TODO(satvik): Handle malformed expression*)
        Error (InvalidToken ((Printf.sprintf "Encountered invalid token (%s) at line %d\n" hd.lexeme hd.line), Some hd))
    | _ ->
        Error (InvalidToken ("Ran out of tokens, expected a primary expression.", None))

(** Parses unary grammar *)
and parse_unary (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    match tokens with
    | { token_type = Lexer.Bang; _ } :: tl ->
        parse_unary tl >>= fun (unary, rest) -> 
            Ok ((Unary (Bang, unary)), rest)

    | { token_type = Lexer.Minus; _ } :: tl ->
        parse_unary tl >>= fun (unary, rest) -> 
            Ok ((Unary (Minus, unary)), rest)

    | _ -> parse_primary tokens

(** Parses factor grammar *)
and parse_factor (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    parse_unary tokens >>= fun (left, rest) -> 
        let rec p (e: expr) (t: Lexer.token list): (expr * Lexer.token list, parser_error) result =
            (match t with
        | { token_type = Lexer.Slash; _ } :: tl ->
            parse_unary tl >>= fun (right, rest') ->
                p (Binary (e, Slash, right)) rest'

        | { token_type = Lexer.Star; _ } :: tl ->
            parse_unary tl >>= fun (right, rest') -> 
                p (Binary (e, Star, right)) rest'

        | _ -> Ok (e, t))
        in p left rest

(** Parses term grammar *)
and parse_term (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    parse_factor tokens >>= fun (left, rest) -> 
        let rec p (e: expr) (t : Lexer.token list): (expr * Lexer.token list, parser_error) result =
            (
                match t with
                | { token_type = Lexer.Plus; _ } :: tl ->
                    parse_factor tl >>= fun (right, rest') ->
                        p (Binary (e, Plus, right)) rest'

                | { token_type = Lexer.Minus; _ } :: tl ->
                    parse_factor tl >>= fun (right, rest') ->
                        p (Binary (e, Minus, right)) rest'

                | _ -> Ok (e, t)
            )

        in p left rest

(** Parses comparison grammar *)
and parse_comparison (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    parse_term tokens >>= fun (left, rest) -> 
        let rec p (e: expr) (t : Lexer.token list): (expr * Lexer.token list, parser_error) result =
            (
                match t with
                | { token_type = Lexer.Greater; _ } :: tl ->
                    parse_term tl >>= fun (right, rest') -> 
                        p (Binary (e, Greater, right)) rest'

                | { token_type = Lexer.GreaterEqual; _ } :: tl ->
                    parse_term tl >>= fun (right, rest') -> 
                        p (Binary (e, GreaterEqual, right)) rest'

                | { token_type = Lexer.Less; _ } :: tl ->
                    parse_term tl >>= fun (right, rest') -> 
                        p (Binary (e, Less, right)) rest'

                | { token_type = Lexer.LessEqual; _ } :: tl ->
                    parse_term tl >>= fun (right, rest') -> 
                        p (Binary (e, LessEqual, right)) rest'

                | _ -> Ok (e, t)
            )

        in p left rest


(** Parses equality grammar *)
and parse_equality (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    parse_comparison tokens >>= fun (left, rest) -> 
        let rec p (e: expr) (t : Lexer.token list): (expr * Lexer.token list, parser_error) result =
            (
                match t with
                | { token_type = Lexer.BangEqual; _ } :: tl ->
                    parse_factor tl >>= fun (right, rest') -> 
                        p (Binary (e, NotEqual, right)) rest'

                | { token_type = Lexer.EqualEqual; _ } :: tl ->
                    parse_factor tl >>= fun (right, rest') -> 
                        p (Binary (e, EqualEqual, right)) rest'

                | _ -> Ok (e, t)
            )

        in p left rest

and parse_expression (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    parse_equality tokens

(** Recursively evaluates an expression. *)
let rec eval_expression (e : expr): (expr, eval_error) result =
    match e with 
    | Literal lit -> Ok (Literal lit)

    | Unary (Bang, expr) ->
        eval_expression expr >>= fun inner -> (
            match inner with 
            | Literal False -> Ok (Literal True)
            | Literal True -> Ok (Literal False)
            | _ -> Error (TypeError ("Invlaid unary operation encountered", Unary (Bang, expr)))
        )

    | Unary (Minus, expr) ->
        eval_expression expr >>= fun inner ->  (
            match inner with 
            | (Literal Number n) -> Ok (Literal (Number (-1. *. n)))
            | _ -> Error (TypeError ("Invalid unary operation encountered", Unary (Minus, expr)))
        )

    | Binary (e1, op, e2) ->
        eval_expression e1 >>= fun left ->
            eval_expression e2 >>= fun right -> (
                match (left, op, right) with 
                    | Literal l, EqualEqual, Literal r -> if l = r then Ok (Literal True) else Ok (Literal False)
                    | Literal l, NotEqual, Literal r -> if l = r then Ok (Literal False) else Ok (Literal True)

                    | Literal l, Less, Literal r -> if l < r then Ok (Literal True) else Ok (Literal False)
                    | Literal l, Greater, Literal r -> if l > r then Ok (Literal False) else Ok (Literal True)

                    | Literal l, LessEqual, Literal r -> if l <= r then Ok (Literal True) else Ok (Literal False)
                    | Literal l, GreaterEqual, Literal r -> if l >= r then Ok (Literal False) else Ok (Literal True)

                    | Literal Number n1, Plus, Literal Number n2 -> Ok (Literal (Number (n1 +. n2)))
                    | Literal Number n1, Minus, Literal Number n2 -> Ok (Literal (Number (n1 -. n2)))
                    | Literal Number n1, Star, Literal Number n2 -> Ok (Literal (Number (n1 *. n2)))
                    | Literal Number n1, Slash, Literal Number n2 -> Ok (Literal (Number (n1 /. n2)))

                    | _ -> Error (TypeError ("Invalid binary operand encountered", (Binary (e1, op, e2))))
            )

    | Grouping (expr) -> eval_expression expr

let rec parse_program (tokens : Lexer.token list): (unit, parser_error) result = 
    let rec parse (toks : Lexer.token list): (unit, parser_error) result = 
        parse_statement toks >>= fun (_, rest) -> (
            match rest with 
            | [{ token_type = Lexer.EOF; _ }] -> Ok ()
            | [] -> Error ExpectedEOF
            | _ -> parse rest
        )
    in
    parse tokens

and parse_statement (tokens: Lexer.token list): (expr option * Lexer.token list, parser_error) result = 
    match tokens with
    | { token_type = Lexer.Print; _ } :: tl ->
        parse_expression tl >>= fun (expr, toks) -> (
            match toks with 
            | { token_type = Lexer.Semicolon; _ } :: tl' -> (
                match eval_expression expr with 
                | Ok Literal String s -> Printf.printf "%s" s; Ok (None, tl')
                | Ok Literal Number n -> Printf.printf "%f" n ; Ok (None, tl')
                | Error eval_error -> Error (EvalError eval_error)
                | _ -> Error PrintInvalidExpression
            )
            | _ -> Error MissingSemicolon
        );

    | _ ->
        parse_expression tokens >>= fun (expr, toks) -> (
            match toks with 
            | { token_type = Lexer.Semicolon; _ } :: tl' ->
                Ok (Some expr, tl')
            | _ -> Error MissingSemicolon
        )
            
