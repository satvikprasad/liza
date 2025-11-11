let run (source : string) =
    let tokens = Liza.Lexer.scan_tokens source in
    let expr, _ = Liza.Parser.parse_expression tokens in 
    Liza.Lexer.tokens_to_string tokens |> Printf.printf "%s\n";
    Liza.Parser.pretty_print_expr expr |> Printf.printf "%s\n"

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
