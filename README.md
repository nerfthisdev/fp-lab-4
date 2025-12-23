# ocsh

OCaml-powered shell runner inspired by babashka. You can run OCaml scripts, drop into a REPL, and call Linux binaries directly from OCaml without boilerplate.

## What it does

- Runs OCaml files, `-e` expressions, or stdin.
- Provides a REPL that accepts OCaml phrases (`;;`) and shell commands (`;;`).
- Lets you call binaries as OCaml functions: `ls "bin"`, `cat "file.txt"`.
- Provides a small command monad for chaining shell steps.

## Quick start

Build:

```
dune build
```

Run a script:

```
cat > hello.ocsh <<'EOS'
let name = "world";;
print_endline ("hello " ^ name);;

let listing = sh "ls -1";;
print_endline listing;;
EOS

dune exec ocsh -- hello.ocsh
```

Inline expression:

```
dune exec ocsh -- -e "print_endline (sh \"printf hi\")"
```

Start the REPL:

```
dune exec ocsh
```

In the REPL, end commands with `;;`:

```
ls ;;
cd .. ;;
pwd ;;
```

OCaml phrases still work:

```
let x = "bin";;
ls x;;
print_endline x;;
```

## How it works

- The REPL reads until `;;`.
- It first tries to parse the input as OCaml.
- If OCaml parsing fails with an unbound command, it rewrites `cmd arg1 arg2` into a shell call.
- If the line is not OCaml and not a rewrite, it is executed as a shell line.

## Usage scenarios

List files and reuse the output:

```
let files = ls ".";;
print_string files;;
```

Echo OCaml variables:

```
let x = "hello";;
echo x;;
```

Functional pipeline style:

```
let count =
  ps
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "")
  |> List.length
;;
Printf.printf "lines: %d\n" count;;
```

For pipelines, assign the command result first so the rewrite can wrap it:

```
let count =
  ps
  |> String.split_on_char '\n'
  |> List.length
;;
```

Change directories and run commands:

```
cd "test";;
ls ".";;
pwd ();;
```

Chain commands with the monad:

```
let open Cmd in
let* who = cmd "whoami" [] in
let* home = cmd "printf" [Sys.getenv "HOME"] in
return (who ^ ":" ^ home);;
```

Handle failures explicitly:

```
let open Cmd in
match run (cmd "false" []) with
| Ok _ -> print_endline "ok"
| Error msg -> prerr_endline ("failed: " ^ msg);;
```

Mix OCaml logic with shell commands:

```
let files = ls ".";;
let lines = String.split_on_char '\n' files;;
let count = List.length (List.filter (fun s -> s <> "") lines);;
Printf.printf "files: %d\n" count;;
```

## Prelude API

The toplevel defines these helpers by default so they are in scope:

- `run ?cwd ?input cmd` -> `{ status; stdout; stderr }`
- `sh ?cwd cmd` -> `stdout` (raises on non-zero exit)
- `cmd ?cwd prog args` -> `run` on a quoted argument list
- `bin ?cwd prog args` -> `stdout` convenience for `cmd`
- `cd dir` and `pwd ()` helpers
- `echo s` -> prints an OCaml string with a newline

Example:

```
let res = run "uname -a";;
print_endline res.stdout;;

let out = sh "printf 'hi'";;
print_endline out;;

let res2 = cmd "printf" ["hello"; "\n"];;
print_endline res2.stdout;;
```

## Command monad

Use `Prelude.Cmd` to chain commands with `let*`:

```
let open Cmd in
let* out = cmd "printf" ["hi"] in
return out
;;
```

## Tests

```
dune runtest
```

## Project layout

- `bin/ocsh.ml` CLI entry point
- `lib/ocsh_runtime.ml` shell helpers + prelude
- `lib/ocsh_parser.ml` phrase splitter + command rewriter
- `lib/runner.ml` OCaml toplevel evaluator + rewrite
- `test/test_ocsh.ml` Alcotest suite
