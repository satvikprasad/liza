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
    | Identifier of string
    | Assignment of string * expr

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

    | Identifier name -> Printf.sprintf "%s" name

    | Assignment (var, expr) -> Printf.sprintf "(= %s %s)" var (pretty_print_expr expr)

exception GroupException of string
exception InvalidTokenException of string

type eval_error =
    | TypeError of string * expr
    | IdentifierNotFound of string * expr
    | LookupError of string
    | AssignmentError of string

type parser_error =
    | InvalidToken of string * Lexer.token option
    | MissingSemicolon
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

    | { token_type = Lexer.Identifier; lexeme; _ } :: tl -> Ok (Identifier lexeme, tl)

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

and parse_assignment (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result = 
    match tokens with 
    | { token_type = Lexer.Identifier; lexeme = id_lex ; _ } :: { token_type = Lexer.Equal; _ } :: tl ->
        parse_assignment tl >>= fun (expr, rest) -> Ok (Assignment (id_lex, expr), rest)
    | _ -> parse_equality tokens

and parse_expression (tokens: Lexer.token list): (expr * Lexer.token list, parser_error) result =
    parse_assignment tokens

type environment = { 
    values: (string, literal) Hashtbl.t;
    enclosing: environment option
}

let global_env = { values = Hashtbl.create 64; enclosing = None }

let rec env_lookup (env : environment) (id : string): (literal, eval_error) result = 
    try 
        Ok (Hashtbl.find env.values id)
    with 
    | Not_found -> 
        match env.enclosing with 
        | Some e -> env_lookup e id
        | None -> Error (LookupError id)

let rec env_assign (env : environment) (id : string) (lit : literal): (literal, eval_error) result = 
    if Hashtbl.mem env.values id then (
        Hashtbl.replace env.values id lit;
        Ok (lit)
    ) else (
        match env.enclosing with 
        | Some e -> env_assign e id lit
        | None -> Error (AssignmentError id)
    )

let create_scoped_env (parent: environment) =
    { values = Hashtbl.create 64; enclosing = Some parent } 

(** Recursively evaluates an expression into a literal. *)
let rec eval_expression (e : expr) (env : environment): (literal, eval_error) result =
    match e with
    | Literal lit -> Ok (lit)

    | Unary (Bang, expr) ->
        eval_expression expr env >>= fun inner -> (
            match inner with
            | False -> Ok (True)
            | True -> Ok (False)
            | _ -> Error (TypeError ("Invlaid unary operation encountered", Unary (Bang, expr)))
        )

    | Unary (Minus, expr) ->
        eval_expression expr env >>= fun inner ->  (
            match inner with
            | (Number n) -> Ok ((Number (-1. *. n)))
            | _ -> Error (TypeError ("Invalid unary operation encountered", Unary (Minus, expr)))
        )

    | Binary (e1, op, e2) ->
        eval_expression e1 env >>= fun left ->
            eval_expression e2 env >>= fun right -> (
                match (left, op, right) with
                    | l, EqualEqual, r -> if l = r then Ok (True) else Ok (False)
                    | l, NotEqual, r -> if l = r then Ok (False) else Ok (True)

                    | l, Less, r -> if l < r then Ok (True) else Ok (False)
                    | l, Greater, r -> if l > r then Ok (False) else Ok (True)

                    | l, LessEqual, r -> if l <= r then Ok (True) else Ok (False)
                    | l, GreaterEqual, r -> if l >= r then Ok (False) else Ok (True)

                    | Number n1, Plus, Number n2 -> Ok ((Number (n1 +. n2)))

                    | String s1, Plus, String s2 -> Ok ((String (s1 ^ s2)))

                    | String s1, Plus, Number n1 -> Ok ((String (Printf.sprintf "%s%g" s1 n1)))

                    | Number n1, Plus, String s1 -> Ok ((String (Printf.sprintf "%g%s" n1 s1)))

                    | Number n1, Minus, Number n2 -> Ok ((Number (n1 -. n2)))
                    | Number n1, Star, Number n2 -> Ok ((Number (n1 *. n2)))
                    | Number n1, Slash, Number n2 -> Ok ((Number (n1 /. n2)))

                    | _ -> Error (TypeError ("Invalid binary operand encountered", (Binary (e1, op, e2))))
            )

    | Grouping expr -> eval_expression expr env

    | Identifier id -> env_lookup env id

    | Assignment (id, expr) -> eval_expression expr env >>= fun lit -> env_assign env id lit

type statement = 
    | VarDecl of string * expr
    | Print of expr
    | Expression of expr
    | Block of statement list

let rec eval_statement (s : statement) (env : environment): (unit, eval_error) result =
    match s with 
    | VarDecl (id, expr) -> eval_expression expr env >>= fun l -> (
        Ok (Hashtbl.add env.values id l)
    )
    | Print expr -> eval_expression expr env >>= fun l -> (
        match l with 
        | String s -> Printf.printf "%s\n" s
        | Number n -> Printf.printf "%g\n" n
        | True -> Printf.printf "true\n"
        | False -> Printf.printf "false\n"
        | Nil -> Printf.printf "nil\n"
    ); Ok()
    | Expression e -> eval_expression e env >>= fun _ -> Ok ()
    | Block statements -> (
        let scoped_env = create_scoped_env env in

        let rec p (stats : statement list) : (unit, eval_error) result = 
            match stats with 
            | s :: rest -> eval_statement s scoped_env >>= fun () -> p rest
            | _ -> Ok ()

        in p statements
    )


let rec parse_program (tokens : Lexer.token list): (statement list, parser_error) result =
    let rec parse (toks : Lexer.token list): (statement list, parser_error) result =
        parse_statement toks >>= fun (stat, rest) -> (
            match rest with
            | [{ token_type = Lexer.EOF; _ }] -> Ok [stat]
            | [] -> Error ExpectedEOF
            | _ -> parse rest >>= fun (statements) -> Ok (stat :: statements)
        )
    in
    parse tokens

and parse_statement (tokens : Lexer.token list): (statement * Lexer.token list, parser_error) result =
    match tokens with
    | { token_type = Lexer.Var; _ } :: { token_type = Lexer.Identifier; lexeme = id_lex; _ } :: tl -> (
        match tl with
        | { token_type = Lexer.Equal; _ } :: tl' -> 
            parse_expression tl' >>= fun (right, rest) -> (
                match rest with
                | { token_type = Lexer.Semicolon; _ } :: rest' -> Ok (VarDecl (id_lex, right), rest')
                | _ -> Error (MissingSemicolon)
            )

        | { token_type = Lexer.Semicolon; _ } :: tl' ->
            Ok (VarDecl (id_lex, Literal Nil), tl') (* Declared but not defined, choose to assign it a nil expression *)

        | tok :: _ -> Error (InvalidToken ("Expected semicolon or assignment operator.", Some tok))

        | _ -> Error (InvalidToken ("Expected semicolon or assinment operator.", None))
    )
    | { token_type = Lexer.Print; _ } :: tl ->
        parse_expression tl >>= fun (expr, toks) -> (
            match toks with
            | { token_type = Lexer.Semicolon; _ } :: tl' -> Ok (Print expr, tl')
            | _ -> Error MissingSemicolon
        );

    | { token_type = Lexer.LeftBrace; _} :: tl ->
        let rec p (tokens: Lexer.token list) : (statement list * Lexer.token list, parser_error) result = 
            match tokens with
            | { token_type = Lexer.RightBrace; _ } :: tl' -> Ok ([], tl')
            | _ -> parse_statement tokens >>= fun (stat, rest) -> (
                p rest >>= fun (rem_stats, rest') -> (
                    Ok (stat :: rem_stats, rest')
                )
            )
        in p tl >>= fun (statements, toks) -> (
            Ok (Block statements, toks)
        )
    | _ ->
        parse_expression tokens >>= fun (expr, toks) -> (
            match toks with
            | { token_type = Lexer.Semicolon; _ } :: tl' ->
                Ok (Expression expr, tl')
            | _ -> Error MissingSemicolon
        )
