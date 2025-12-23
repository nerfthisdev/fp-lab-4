# ocsh

OCaml-powered shell runner inspired by babashka. You get a REPL that understands OCaml and can call Linux binaries directly, plus a script runner for `.ocsh` files and `-e` expressions.

## What this is for

- Use OCaml as a shell scripting language.
- Combine real shell commands with OCaml parsing, filtering, and composition.
- Keep command results as strings and transform them with functional pipelines.

## How it works (exact flow)

1) The REPL reads input until it sees `;;`.
2) It tries to parse and execute the phrase as OCaml.
3) If OCaml fails with `Unbound value <cmd>`, the phrase is rewritten as a shell call:
   - `ls "bin"` becomes `__ocsh_bin "ls" ["bin"]`
   - `let x = ps` becomes `let x = __ocsh_bin "ps" []`
   - `ps |> f` becomes `(__ocsh_bin "ps" []) |> f`
4) If rewriting fails and the phrase does not look like OCaml, the line is executed as a raw shell command.
5) The prelude injects helpers (`sh`, `cmd`, `bin`, `echo`, etc.) into the toplevel so they are always in scope.

This gives you a shell with OCaml syntax and functional composition while still letting you run standard Linux tools.

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

In the REPL, end every phrase with `;;`:

```
ls ;;
cd .. ;;
pwd ();;
```

OCaml phrases still work:

```
let x = "bin";;
ls x;;
print_endline x;;
```

## Usage examples

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
let* host = cmd "hostname" [] in
return (who ^ "@" ^ host);;
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
