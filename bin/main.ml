let run (source : string) =
    Liza.Lexer.scan_tokens source

let _run_file file =
    run (In_channel.with_open_bin file In_channel.input_all)

let rec _run_repl () =
    try
        Printf.printf "> ";
        flush stdout;

        let input = read_line () in
            (* Run input *)

        Printf.printf "%s\n" (Liza.Lexer.tokens_to_string (run input));

        _run_repl ()
    with End_of_file ->
        Printf.printf "\nExiting...\n"

let () =
    Liza.Parser.Unary (
        Liza.Parser.Bang,
        Liza.Parser.Grouping (
            Liza.Parser.Binary (
                (Liza.Parser.Literal (Liza.Parser.String "Hello")),
                Liza.Parser.Plus,
                (Liza.Parser.Literal (Liza.Parser.Number 42.3))
            )
        )
    )
    |> Liza.Parser.pretty_print_expr
    |> Printf.printf "%s\n"
(*
    match Sys.argv with
    | [|_; file|] ->
        Printf.printf "%s\n" (Liza.Lexer.tokens_to_string (run_file file));
    | [|_|] -> run_repl ()
    | _ -> Printf.printf "Usage: liza [script]\n"
    *)
