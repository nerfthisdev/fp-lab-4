type arg_token =
  | String_lit of string
  | Ident of string

type command = {
  assign : string option;
  name : string;
  args : arg_token list;
  suffix : string option;
}

let is_ident_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

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

let parse_command ~unbound code =
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
    let assign, after_let =
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
            let rest = String.sub without_semis pos (String.length without_semis - pos) in
            let rest_trimmed = String.trim rest in
            if rest_trimmed = "" then
              Some { assign; name = cmd; args; suffix = None }
            else if String.starts_with ~prefix:"|>" rest_trimmed then
              Some { assign; name = cmd; args; suffix = Some rest }
            else
              None)
    | _ -> None

let arg_to_source = function
  | String_lit arg -> Printf.sprintf "\"%s\"" (String.escaped arg)
  | Ident name -> name

let rewrite_command cmd =
  let args_source = cmd.args |> List.map arg_to_source |> String.concat "; " in
  let list_source = "[" ^ args_source ^ "]" in
  let cmd_source = Printf.sprintf "\"%s\"" (String.escaped cmd.name) in
  let base = Printf.sprintf "__ocsh_bin %s %s" cmd_source list_source in
  let expr =
    match cmd.suffix with
    | None -> base
    | Some suffix -> "(" ^ base ^ ")" ^ suffix
  in
  match cmd.assign with
  | Some name -> Printf.sprintf "let %s = %s;;" name expr
  | None -> (
      match cmd.suffix with
      | None ->
          Printf.sprintf
            "let __out = %s in print_string __out; flush stdout;;"
            expr
      | Some _ -> Printf.sprintf "let _ = %s in ();;" expr)

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
  let phrases = if tail = "" then !phrases else tail :: !phrases in
  List.rev phrases

let is_ocaml_start line =
  let trimmed = String.trim line in
  trimmed = ""
  || List.exists
       (fun kw ->
         let prefix = kw ^ " " in
         trimmed = kw || String.starts_with ~prefix trimmed)
       [ "let"
       ; "type"
       ; "module"
       ; "open"
       ; "if"
       ; "match"
       ; "for"
       ; "while"
       ; "fun"
       ; "function"
       ; "try"
       ; "class"
       ; "object"
       ; "exception"
       ; "include"
       ]
