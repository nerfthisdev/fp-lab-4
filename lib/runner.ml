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

let is_ident_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

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
        else if is_ident_char msg.[i] then stop (i + 1)
        else i
      in
      let finish = stop start in
      if finish = start then None
      else Some (String.sub msg start (finish - start))

let skip_spaces s i =
  let len = String.length s in
  let rec loop j =
    if j >= len then j
    else
      match s.[j] with
      | ' ' | '\t' | '\n' | '\r' -> loop (j + 1)
      | _ -> j
  in
  loop i

let parse_ident s i =
  let len = String.length s in
  if i >= len then None
  else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
        let rec loop j =
          if j >= len then j
          else if is_ident_char s.[j] then loop (j + 1)
          else j
        in
        let j = loop (i + 1) in
        Some (String.sub s i (j - i), j)
    | _ -> None

let parse_string_literal s i =
  let len = String.length s in
  if i >= len || s.[i] <> '"' then None
  else
    let buffer = Buffer.create 16 in
    let rec loop j =
      if j >= len then None
      else
        match s.[j] with
        | '"' -> Some (Buffer.contents buffer, j + 1)
        | '\\' ->
            if j + 1 >= len then None
            else (
              match s.[j + 1] with
              | '\\' ->
                  Buffer.add_char buffer '\\';
                  loop (j + 2)
              | '"' ->
                  Buffer.add_char buffer '"';
                  loop (j + 2)
              | 'n' ->
                  Buffer.add_char buffer '\n';
                  loop (j + 2)
              | 't' ->
                  Buffer.add_char buffer '\t';
                  loop (j + 2)
              | other ->
                  Buffer.add_char buffer other;
                  loop (j + 2))
        | other ->
            Buffer.add_char buffer other;
            loop (j + 1)
    in
    loop (i + 1)

type arg_token =
  | String_lit of string
  | Ident of string

let parse_arg s i =
  match parse_string_literal s i with
  | Some (value, next) -> Some (String_lit value, next)
  | None -> (
      match parse_ident s i with
      | Some (name, next) -> Some (Ident name, next)
      | None -> None)

let rec parse_args s i acc =
  let i = skip_spaces s i in
  if i >= String.length s then Some (List.rev acc, i)
  else
    match parse_arg s i with
    | None -> Some (List.rev acc, i)
    | Some (arg, j) -> parse_args s j (arg :: acc)

let strip_suffix s suffix =
  let len = String.length s in
  let slen = String.length suffix in
  if len >= slen && String.sub s (len - slen) slen = suffix then
    String.sub s 0 (len - slen)
  else
    s

let try_rewrite_command ~unbound code =
  let trimmed = String.trim code in
  if trimmed = "" then None
  else
    let without_semis = String.trim (strip_suffix trimmed ";;") in
    let i = skip_spaces without_semis 0 in
    let has_let =
      match parse_ident without_semis i with
      | Some ("let", j) -> Some j
      | _ -> None
    in
    let var_name, after_let =
      match has_let with
      | None -> (None, i)
      | Some j -> (
          match parse_ident without_semis (skip_spaces without_semis j) with
          | Some (name, k) ->
              let k = skip_spaces without_semis k in
              if k < String.length without_semis && without_semis.[k] = '=' then
                (Some name, k + 1)
              else
                (None, i)
          | None -> (None, i))
    in
    let after_let = skip_spaces without_semis after_let in
    match parse_ident without_semis after_let with
    | Some (cmd, j) when cmd = unbound -> (
        match parse_args without_semis j [] with
        | None -> None
        | Some (args, pos) ->
            let pos = skip_spaces without_semis pos in
            if pos <> String.length without_semis then None
            else
              let arg_to_source = function
                | String_lit arg ->
                    Printf.sprintf "\"%s\"" (String.escaped arg)
                | Ident name -> name
              in
              let args_source =
                args |> List.map arg_to_source |> String.concat "; "
              in
              let list_source = "[" ^ args_source ^ "]" in
              let cmd_source = Printf.sprintf "\"%s\"" (String.escaped cmd) in
              let expr =
                match var_name with
                | Some name ->
                    Printf.sprintf "let %s = __ocsh_bin %s %s;;" name
                      cmd_source list_source
                | None ->
                    Printf.sprintf
                      "let __out = __ocsh_bin %s %s in print_string __out; \
                       flush stdout;;"
                      cmd_source list_source
              in
              Some expr)
    | _ -> None

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

let eval_string ?(verbose = false) code =
  init ();
  let split_phrases input =
    let len = String.length input in
    let buffer = Buffer.create len in
    let phrases = ref [] in
    let in_string = ref false in
    let i = ref 0 in
    while !i < len do
      let c = input.[!i] in
      if !in_string then (
        Buffer.add_char buffer c;
        if c = '\\' && !i + 1 < len then (
          incr i;
          Buffer.add_char buffer input.[!i])
        else if c = '"' then in_string := false)
      else if c = '"' then (
        in_string := true;
        Buffer.add_char buffer c)
      else if c = ';' && !i + 1 < len && input.[!i + 1] = ';' then (
        Buffer.add_string buffer ";;";
        phrases := Buffer.contents buffer :: !phrases;
        Buffer.clear buffer;
        incr i)
      else
        Buffer.add_char buffer c;
      incr i
    done;
    let tail = Buffer.contents buffer in
    let tail =
      let trimmed = String.trim tail in
      if trimmed = "" then ""
      else if String.length trimmed >= 2
              && String.sub trimmed (String.length trimmed - 2) 2 = ";;"
      then tail
      else tail ^ ";;"
    in
    let phrases =
      if tail = "" then !phrases else tail :: !phrases
    in
    List.rev phrases
  in
  let rec eval_phrases = function
    | [] -> Ok ()
    | phrase :: rest ->
        let trimmed = String.trim phrase in
        if trimmed = "" then eval_phrases rest
        else
          (match eval_phrase_string_with_rewrite ~verbose phrase with
          | Ok -> eval_phrases rest
          | Incomplete -> Error "incomplete phrase"
          | Error msg -> Error msg)
  in
  eval_phrases (split_phrases code)

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
  | Ok
  | Incomplete
  | Error of string

let eval_phrase_string ?(verbose = false) code =
  init ();
  let lexbuf = Lexing.from_string code in
  try
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    let ok = Toploop.execute_phrase verbose Format.std_formatter phrase in
    if ok then Ok else Error "toplevel execution failed"
  with
  | End_of_file -> Incomplete
  | Env.Error err -> Error (Format.asprintf "%a" Env.report_error err)
  | exn ->
      let message =
        try Format.asprintf "%a" Location.report_exception exn
        with _ -> Printexc.to_string exn
      in
      Error message

let eval_phrase_string_with_rewrite ?(verbose = false) code =
  match eval_phrase_string ~verbose code with
  | Ok -> Ok
  | Incomplete -> Incomplete
  | Error msg -> (
      match extract_unbound_value msg with
      | None -> Error msg
      | Some unbound -> (
          match try_rewrite_command ~unbound code with
          | None -> Error msg
          | Some rewritten -> eval_phrase_string ~verbose rewritten))
