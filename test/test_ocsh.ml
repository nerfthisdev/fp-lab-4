let check_run () =
  let open Ocsh_lib.Ocsh_runtime.Prelude in
  let result = run "printf 'hi'" in
  Alcotest.(check int) "status" 0 result.status;
  Alcotest.(check string) "stdout" "hi" result.stdout

let check_sh_failure () =
  let open Ocsh_lib.Ocsh_runtime.Prelude in
  let raised =
    try
      let _ = sh "exit 2" in
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "sh fails" true raised

let check_eval_string () =
  match Ocsh_lib.Runner.eval_string "let x = 1 + 1;;" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_eval_string_no_semis () =
  match Ocsh_lib.Runner.eval_string "let x = 1 + 2" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_eval_shell_rewrite () =
  match Ocsh_lib.Runner.eval_string "let x = printf \"ok\";;" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_eval_shell_rewrite_ident () =
  match Ocsh_lib.Runner.eval_string "let y = \"bin\";; let x = ls y;;" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_echo () =
  match Ocsh_lib.Runner.eval_string "let x = \"hi\";; echo x;;" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_cmd_monad () =
  let open Ocsh_lib.Ocsh_runtime.Prelude.Cmd in
  let action =
    let* out = cmd "printf" [ "ok" ] in
    return out
  in
  match run action with
  | Ok value -> Alcotest.(check string) "monad output" "ok" value
  | Error msg -> Alcotest.fail msg

let check_split_phrases () =
  let input = "let x = 1;; let y = \"a;;b\";;" in
  let phrases = Ocsh_lib.Ocsh_parser.split_phrases input in
  Alcotest.(check int) "phrase count" 2 (List.length phrases);
  match phrases with
  | [ first; second ] ->
      Alcotest.(check bool) "first ends" true
        (String.ends_with ~suffix:";;" first);
      Alcotest.(check bool) "second ends" true
        (String.ends_with ~suffix:";;" second)
  | _ -> Alcotest.fail "unexpected phrase split"

let check_eval_shell_rewrite_pipe () =
  let code =
    "let count = ps |> String.split_on_char '\\n' |> List.length;;"
  in
  match Ocsh_lib.Runner.eval_string code with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_eval_shell_rewrite_pipe_args () =
  let code =
    "let count = ls \".\" |> String.split_on_char '\\n' |> List.length;;"
  in
  match Ocsh_lib.Runner.eval_string code with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_cmd_monad_map () =
  let open Ocsh_lib.Ocsh_runtime.Prelude.Cmd in
  let action =
    let+ out = cmd "printf" [ "ok" ] in
    String.uppercase_ascii out
  in
  match run action with
  | Ok value -> Alcotest.(check string) "monad map" "OK" value
  | Error msg -> Alcotest.fail msg

let check_cmd_monad_fail () =
  let open Ocsh_lib.Ocsh_runtime.Prelude.Cmd in
  let action =
    let* _ = cmd "false" [] in
    return "ok"
  in
  match run action with
  | Ok _ -> Alcotest.fail "expected failure"
  | Error _ -> ()

let check_eval_multi_phrases () =
  let code = "let x = ps;; let y = ls \".\";;" in
  match Ocsh_lib.Runner.eval_string code with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_split_phrases_quotes () =
  let input = "let x = \"a\\\\\";;b\";; let y = 1;;" in
  let phrases = Ocsh_lib.Ocsh_parser.split_phrases input in
  Alcotest.(check int) "phrase count" 2 (List.length phrases)

let check_parse_command_pipe () =
  let open Ocsh_lib.Ocsh_parser in
  match parse_command ~unbound:"ps" "let x = ps |> String.length;;" with
  | Some cmd -> (
      match cmd.suffix with
      | Some _ -> ()
      | None -> Alcotest.fail "expected suffix")
  | None -> Alcotest.fail "expected command"

let check_parse_command_reject () =
  let open Ocsh_lib.Ocsh_parser in
  match parse_command ~unbound:"ps" "ps | not_a_pipe" with
  | Some _ -> Alcotest.fail "should reject"
  | None -> ()

let check_eval_pipe_no_assign () =
  let code = "ps |> String.split_on_char '\\n' |> List.length;;" in
  match Ocsh_lib.Runner.eval_string code with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_eval_prelude () =
  match Ocsh_lib.Runner.eval_string "let _ = sh \"printf ok\";;" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg

let check_eval_error () =
  match Ocsh_lib.Runner.eval_string "let = ;;" with
  | Ok () -> Alcotest.fail "expected parse error"
  | Error _ -> ()

let () =
  let open Alcotest in
  run "ocsh"
    [ ( "runtime",
        [ test_case "run" `Quick check_run
        ; test_case "sh failure" `Quick check_sh_failure
        ] )
    ; ( "runner",
        [ test_case "eval string" `Quick check_eval_string
        ; test_case "eval string no ;;"
            `Quick check_eval_string_no_semis
        ; test_case "eval shell rewrite"
            `Quick check_eval_shell_rewrite
        ; test_case "eval shell rewrite ident"
            `Quick check_eval_shell_rewrite_ident
        ; test_case "eval prelude" `Quick check_eval_prelude
        ; test_case "eval error" `Quick check_eval_error
        ; test_case "command monad" `Quick check_cmd_monad
        ; test_case "split phrases" `Quick check_split_phrases
        ; test_case "echo" `Quick check_echo
        ; test_case "eval shell rewrite pipe" `Quick check_eval_shell_rewrite_pipe
        ; test_case "eval shell rewrite pipe args"
            `Quick check_eval_shell_rewrite_pipe_args
        ; test_case "command monad map" `Quick check_cmd_monad_map
        ; test_case "command monad fail" `Quick check_cmd_monad_fail
        ; test_case "eval multi phrases" `Quick check_eval_multi_phrases
        ; test_case "split phrases quotes" `Quick check_split_phrases_quotes
        ; test_case "parse command pipe" `Quick check_parse_command_pipe
        ; test_case "parse command reject" `Quick check_parse_command_reject
        ; test_case "eval pipe no assign" `Quick check_eval_pipe_no_assign
        ] )
    ]
