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
