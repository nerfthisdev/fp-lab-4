let usage = "ocsh [-e EXPR] [file]"

let read_all ic =
  let buffer = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buffer ic 1024
     done
   with End_of_file -> ());
  Buffer.contents buffer

let () =
  let expr = ref None in
  let file = ref None in
  let set_file path = file := Some path in
  let options =
    [ ("-e", Arg.String (fun s -> expr := Some s), "Evaluate EXPR") ]
  in
  let anon_fun s =
    if !file = None then set_file s
    else raise (Arg.Bad "only one file argument is allowed")
  in
  Arg.parse options anon_fun usage;
  let run_shell_line line =
    let trimmed = String.trim line in
    if trimmed = "" then ()
    else if trimmed = "exit" || trimmed = "quit" then exit 0
    else if trimmed = "pwd" then print_endline (Sys.getcwd ())
    else if String.starts_with ~prefix:"cd " trimmed || trimmed = "cd" then (
      let dir =
        if trimmed = "cd" then Sys.getenv_opt "HOME"
        else
          Some
            (String.trim
               (String.sub trimmed 2 (String.length trimmed - 2)))
      in
      match dir with
      | None -> prerr_endline "cd: HOME not set"
      | Some d -> (
          try Unix.chdir (String.trim d)
          with Unix.Unix_error (err, _, _) ->
            prerr_endline (Unix.error_message err)))
    else
      let output =
        try Ocsh_lib.Ocsh_runtime.Prelude.sh trimmed
        with Failure msg ->
          prerr_endline msg;
          ""
      in
      if output <> "" then (
        print_string output;
        flush stdout)
  in
  let rstrip s =
    let len = String.length s in
    let rec loop i =
      if i < 0 then 0
      else
        match s.[i] with
        | ' ' | '\t' | '\n' | '\r' -> loop (i - 1)
        | _ -> i + 1
    in
    let cut = loop (len - 1) in
    if cut = len then s else String.sub s 0 cut
  in
  let buffer_ends_with_semis buffer =
    let contents = rstrip (Buffer.contents buffer) in
    let len = String.length contents in
    len >= 2 && String.sub contents (len - 2) 2 = ";;"
  in
  let strip_trailing_semis s =
    let trimmed = rstrip s in
    let len = String.length trimmed in
    if len >= 2 && String.sub trimmed (len - 2) 2 = ";;" then
      String.sub trimmed 0 (len - 2)
    else
      trimmed
  in
  let rec repl_loop buffer =
    let prompt = if Buffer.length buffer = 0 then "ocsh> " else "....> " in
    prerr_string prompt;
    flush stderr;
    let line = input_line stdin in
    Buffer.add_string buffer line;
    Buffer.add_char buffer '\n';
    if buffer_ends_with_semis buffer then (
      let content = Buffer.contents buffer in
      let trimmed = strip_trailing_semis content in
      match Ocsh_lib.Runner.eval_phrase_string_with_rewrite content with
      | Ocsh_lib.Runner.Repl_ok ->
          flush stdout;
          Buffer.clear buffer;
          repl_loop buffer
      | Ocsh_lib.Runner.Repl_incomplete -> repl_loop buffer
      | Ocsh_lib.Runner.Repl_error msg ->
          if trimmed <> "" && not (Ocsh_lib.Ocsh_parser.is_ocaml_start trimmed)
          then (
            run_shell_line trimmed;
            Buffer.clear buffer;
            repl_loop buffer)
          else (
            prerr_endline msg;
            Buffer.clear buffer;
            repl_loop buffer))
    else
      repl_loop buffer
  in
  let result =
    match (!expr, !file) with
    | Some code, None -> Ocsh_lib.Runner.eval_string code
    | None, Some path -> Ocsh_lib.Runner.eval_file path
    | None, None -> (
        if Unix.isatty Unix.stdin then (
          let buffer = Buffer.create 256 in
          try repl_loop buffer with End_of_file -> Ok ())
        else
          let code = read_all stdin in
          Ocsh_lib.Runner.eval_string code)
    | Some _, Some _ -> Error "use -e or file, not both"
  in
  match result with
  | Ok () -> ()
  | Error msg ->
      prerr_endline msg;
      exit 1
