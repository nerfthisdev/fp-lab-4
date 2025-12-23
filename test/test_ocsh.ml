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

let check_cmd_monad () =
  let open Ocsh_lib.Ocsh_runtime.Prelude.Cmd in
  let action =
    let* out = cmd "printf" [ "ok" ] in
    return out
  in
  match run action with
  | Ok value -> Alcotest.(check string) "monad output" "ok" value
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
        ] )
    ]
