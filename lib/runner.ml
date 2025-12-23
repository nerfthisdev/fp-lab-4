let rec eval_lexbuf ~verbose lexbuf =
  try
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    let ok = Toploop.execute_phrase verbose Format.std_formatter phrase in
    if ok then eval_lexbuf ~verbose lexbuf
    else Error "toplevel execution failed"
  with
  | End_of_file -> Ok ()
  | Env.Error err ->
      Error (Format.asprintf "%a" Env.report_error err)
  | exn ->
      let message =
        try Format.asprintf "%a" Location.report_exception exn
        with _ -> Printexc.to_string exn
      in
      Error message

let extract_unbound_value msg =
  let prefix = "Unbound value " in
  let len = String.length msg in
  let prefix_len = String.length prefix in
  let rec find i =
    if i + prefix_len > len then None
    else if String.sub msg i prefix_len = prefix then Some (i + prefix_len)
    else find (i + 1)
  in
  match find 0 with
  | None -> None
  | Some start ->
      let rec stop i =
        if i >= len then i
        else if Ocsh_parser.is_ident_char msg.[i] then stop (i + 1)
        else i
      in
      let finish = stop start in
      if finish = start then None
      else Some (String.sub msg start (finish - start))

let try_rewrite_command ~unbound code =
  match Ocsh_parser.parse_command ~unbound code with
  | None -> None
  | Some cmd -> Some (Ocsh_parser.rewrite_command cmd)

let initialized = ref false

let init () =
  if not !initialized then (
    Toploop.initialize_toplevel_env ();
    let stdlib = Config.standard_library in
    Topdirs.dir_directory stdlib;
    Topdirs.dir_directory (Filename.concat stdlib "unix");
    let unix_cma = Filename.concat stdlib "unix/unix.cma" in
    (match
       eval_lexbuf ~verbose:false
         (Lexing.from_string (Printf.sprintf "#load \"%s\";;" unix_cma))
     with
    | Ok () -> ()
    | Error msg -> failwith msg);
    (match
       eval_lexbuf ~verbose:false
         (Lexing.from_string Ocsh_runtime.prelude_source)
     with
    | Ok () -> ()
    | Error msg -> failwith msg);
    initialized := true)

let eval_file ?(verbose = false) path =
  init ();
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with Lexing.pos_fname = path };
  let result = eval_lexbuf ~verbose lexbuf in
  close_in ic;
  result

type repl_result =
  | Repl_ok
  | Repl_incomplete
  | Repl_error of string

let eval_phrase_string ?(verbose = false) code =
  init ();
  let lexbuf = Lexing.from_string code in
  try
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    let ok = Toploop.execute_phrase verbose Format.std_formatter phrase in
    if ok then Repl_ok else Repl_error "toplevel execution failed"
  with
  | End_of_file -> Repl_incomplete
  | Env.Error err -> Repl_error (Format.asprintf "%a" Env.report_error err)
  | exn ->
      let message =
        try Format.asprintf "%a" Location.report_exception exn
        with _ -> Printexc.to_string exn
      in
      Repl_error message

let eval_phrase_string_with_rewrite ?(verbose = false) code =
  match eval_phrase_string ~verbose code with
  | Repl_ok -> Repl_ok
  | Repl_incomplete -> Repl_incomplete
  | Repl_error msg -> (
      match extract_unbound_value msg with
      | None -> Repl_error msg
      | Some unbound -> (
          match try_rewrite_command ~unbound code with
          | None -> Repl_error msg
          | Some rewritten -> eval_phrase_string ~verbose rewritten))

let eval_string ?(verbose = false) code =
  init ();
  let split_phrases = Ocsh_parser.split_phrases in
  let rec eval_phrases = function
    | [] -> Ok ()
    | phrase :: rest ->
        let trimmed = String.trim phrase in
        if trimmed = "" then eval_phrases rest
        else
          (match eval_phrase_string_with_rewrite ~verbose phrase with
          | Repl_ok -> eval_phrases rest
          | Repl_incomplete -> Error "incomplete phrase"
          | Repl_error msg -> Error msg)
  in
  eval_phrases (split_phrases code)
