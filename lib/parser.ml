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

(** Parses primary grammar *)
let rec parse_primary (tokens : Lexer.token list): expr * Lexer.token list =
    match tokens with
    | { token_type = Lexer.String; literal = Some lit; _ } :: tl -> Literal (String lit), tl
    | { token_type = Lexer.Number; literal = Some lit; _ } :: tl -> Literal (Number (Float.of_string lit)), tl
    | { token_type = Lexer.True; _ } :: tl -> Literal True, tl
    | { token_type = Lexer.False; _ } :: tl -> Literal False, tl
    | { token_type = Lexer.Nil; _ } :: tl -> Literal Nil, tl

    | { token_type = Lexer.LeftParen; _ } :: tl ->
        let inner_exp, rest = parse_expression tl in
        (match rest with
        | {token_type = Lexer.RightParen; _} :: rest' -> Grouping inner_exp, rest'
        | _ -> Literal Nil, rest) (* TODO(satvik): Handle malformed expression *)

    | _ -> (* TODO(satvik): Handle malformed expression*)
        Literal Nil, tokens

(** Parses unary grammar *)
and parse_unary (tokens: Lexer.token list): expr * Lexer.token list =
    match tokens with
    | { token_type = Lexer.Bang; _ } :: tl ->
        let unary, rest = parse_unary tl in
        (Unary (Bang, unary)), rest

    | { token_type = Lexer.Minus; _ } :: tl ->
        let unary, rest = parse_unary tl in
        (Unary (Minus, unary)), rest

    | _ -> parse_primary tokens

(** Parses factor grammar *)
and parse_factor (tokens: Lexer.token list): expr * Lexer.token list =
    let left, rest = parse_unary tokens in

    let rec p (e: expr) (t: Lexer.token list): expr * Lexer.token list =
        (match t with
        | { token_type = Lexer.Slash; _ } :: tl ->
            let right, rest' = parse_unary tl in
            p (Binary (e, Slash, right)) rest'

        | { token_type = Lexer.Star; _ } :: tl ->
            let right, rest' = parse_unary tl in
            p (Binary (e, Star, right)) rest'

        | _ -> e, t)
    in
        p left rest

(** Parses term grammar *)
and parse_term (tokens: Lexer.token list): expr * Lexer.token list =
    let left, rest = parse_factor tokens in

    let rec p (e: expr) (t : Lexer.token list): expr * Lexer.token list =
        (
            match t with
            | { token_type = Lexer.Plus; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, Plus, right)) rest'

            | { token_type = Lexer.Minus; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, Minus, right)) rest'

            | _ -> e, t
        )

    in p left rest

(** Parses comparison grammar *)
and parse_comparison (tokens: Lexer.token list): expr * Lexer.token list =
    let left, rest = parse_term tokens in

    let rec p (e: expr) (t : Lexer.token list): expr * Lexer.token list =
        (
            match t with
            | { token_type = Lexer.Greater; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, Greater, right)) rest'

            | { token_type = Lexer.GreaterEqual; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, GreaterEqual, right)) rest'

            | { token_type = Lexer.Less; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, Less, right)) rest'

            | { token_type = Lexer.LessEqual; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, LessEqual, right)) rest'

            | _ -> e, t
        )

    in p left rest

(** Parses equality grammar *)
and parse_equality (tokens: Lexer.token list): expr * Lexer.token list =
    let left, rest = parse_comparison tokens in

    let rec p (e: expr) (t : Lexer.token list): expr * Lexer.token list =
        (
            match t with
            | { token_type = Lexer.BangEqual; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, NotEqual, right)) rest'

            | { token_type = Lexer.EqualEqual; _ } :: tl ->
                let right, rest' = parse_factor tl in
                p (Binary (e, EqualEqual, right)) rest'

            | _ -> e, t
        )

    in p left rest

and parse_expression (tokens: Lexer.token list): expr * Lexer.token list =
    parse_equality tokens
