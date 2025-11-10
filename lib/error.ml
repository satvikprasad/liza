let report (line : int) (where : string) (message : string) =
    Printf.printf "[line %d] Error%s: %s\n" line where message

let error (line : int) (message : string) =
    report line "" message