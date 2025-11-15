let run (source : string) =
    let tokens = Liza.Lexer.scan_tokens source in
    match Liza.Parser.parse_program tokens with
    | Ok (statements) -> (
        List.iter (fun s -> (
            match (Liza.Parser.eval_statement s) with
            | Ok () -> ()
            | Error (Liza.Parser.TypeError (msg, expr)) -> Printf.printf "TypeError: %s [%s]\n" msg (Liza.Parser.pretty_print_expr expr)
            | Error (Liza.Parser.IdentifierNotFound (msg, expr)) -> Printf.printf "IdentifierNotFound: %s [%s]\n" msg (Liza.Parser.pretty_print_expr expr)
        )) statements
    )

    | Error (Liza.Parser.InvalidToken (msg, Some tok)) -> Printf.printf "PARSING ERROR: %s [%s]\n" msg (Liza.Lexer.token_to_string tok)
    | Error (Liza.Parser.InvalidToken (msg, None)) -> Printf.printf "PARSING ERROR: %s\n" msg

    | Error MissingSemicolon -> Printf.printf "PARSING ERROR: Missing semicolon.\n"
    | Error PrintInvalidExpression -> Printf.printf "PARSING ERROR: Expected either a string or a number within print.\n"
    | Error ExpectedEOF -> Printf.printf "Expected EOF, but ran out of tokens."

let run_file file =
    run (In_channel.with_open_bin file In_channel.input_all)

let rec run_repl () =
    try
        Printf.printf "> ";
        flush stdout;

        let input = read_line () in
            (* Run input *)

        run input;

        run_repl ()
    with End_of_file ->
        Printf.printf "\nExiting...\n"

let () =
    match Sys.argv with
    | [|_; file|] ->
        run_file file;
    | [|_|] -> run_repl ()
    | _ -> Printf.printf "Usage: liza [script]\n"
