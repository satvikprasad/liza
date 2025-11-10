let run (source : string) =
    Liza.Lexer.scan_tokens source

let run_file file =
    run (In_channel.with_open_bin file In_channel.input_all)

let rec run_repl () =
    try
        Printf.printf "> ";
        flush stdout;

        let input = read_line () in
            (* Run input *)

        Printf.printf "%s\n" (Liza.Lexer.tokens_to_string (run input));

        run_repl ()
    with End_of_file ->
        Printf.printf "\nExiting...\n"

let () =
    match Sys.argv with
    | [|_; file|] ->
        Printf.printf "%s\n" (Liza.Lexer.tokens_to_string (run_file file));
    | [|_|] -> run_repl ()
    | _ -> Printf.printf "Usage: liza [script]\n"
