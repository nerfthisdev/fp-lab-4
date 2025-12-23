module Prelude = struct
  type run_result = {
    status : int;
    stdout : string;
    stderr : string;
  }

  let read_all ic =
    let buffer = Buffer.create 1024 in
    (try
       while true do
         Buffer.add_channel buffer ic 1024
       done
     with End_of_file -> ());
    Buffer.contents buffer

  let build_cmd ?cwd cmd =
    match cwd with
    | None -> cmd
    | Some dir -> "cd " ^ Filename.quote dir ^ " && " ^ cmd

  let run ?cwd ?input cmd =
    let env = Unix.environment () in
    let command = build_cmd ?cwd cmd in
    let ic, oc, ec = Unix.open_process_full command env in
    (match input with
    | None -> ()
    | Some data ->
        output_string oc data;
        flush oc);
    close_out_noerr oc;
    let stdout = read_all ic in
    let stderr = read_all ec in
    let status =
      match Unix.close_process_full (ic, oc, ec) with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED n -> 128 + n
      | Unix.WSTOPPED n -> 128 + n
    in
    { status; stdout; stderr }

  let sh ?cwd cmd =
    let result = run ?cwd cmd in
    if result.status <> 0 then
      failwith
        (Printf.sprintf "command failed (%d): %s\n%s"
           result.status cmd result.stderr)
    else
      result.stdout

  let cmd ?cwd prog args =
    let parts = prog :: List.map Filename.quote args in
    run ?cwd (String.concat " " parts)

  let bin ?cwd prog args =
    let result = cmd ?cwd prog args in
    result.stdout

  let cd dir = Unix.chdir dir

  let pwd () = Sys.getcwd ()

  let echo s = print_endline s

  let run_raw = run

  module Cmd = struct
    type 'a t = unit -> ('a, string) result

    let return value () = Ok value
    let fail msg () = Error msg

    let bind t f () =
      match t () with
      | Ok value -> f value ()
      | Error _ as err -> err

    let map t f () =
      match t () with
      | Ok value -> Ok (f value)
      | Error _ as err -> err

    let ( let* ) = bind
    let ( let+ ) t f = map t f

    let run t = t ()

    let run_cmd ?cwd cmd () =
      let result = run_raw ?cwd cmd in
      if result.status = 0 then Ok result else Error result.stderr

    let sh ?cwd cmd =
      let+ result = run_cmd ?cwd cmd in
      result.stdout

    let cmd ?cwd prog args =
      let parts = prog :: List.map Filename.quote args in
      sh ?cwd (String.concat " " parts)
  end
end

let prelude_source =
  {|
type run_result = {
  status : int;
  stdout : string;
  stderr : string;
}
;;

let read_all ic =
  let buffer = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buffer ic 1024
     done
   with End_of_file -> ());
  Buffer.contents buffer
;;

let build_cmd ?cwd cmd =
  match cwd with
  | None -> cmd
  | Some dir -> "cd " ^ Filename.quote dir ^ " && " ^ cmd
;;

let run ?cwd ?input cmd =
  let env = Unix.environment () in
  let command = build_cmd ?cwd cmd in
  let ic, oc, ec = Unix.open_process_full command env in
  (match input with
  | None -> ()
  | Some data ->
      output_string oc data;
      flush oc);
  close_out_noerr oc;
  let stdout = read_all ic in
  let stderr = read_all ec in
  let status =
    match Unix.close_process_full (ic, oc, ec) with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in
  { status; stdout; stderr }
;;

let sh ?cwd cmd =
  let result = run ?cwd cmd in
  if result.status <> 0 then
    failwith
      (Printf.sprintf "command failed (%d): %s\n%s"
         result.status cmd result.stderr)
  else
    result.stdout
;;

let cmd ?cwd prog args =
  let parts = prog :: List.map Filename.quote args in
  run ?cwd (String.concat " " parts)
;;

let __ocsh_bin ?cwd prog args =
  let result = cmd ?cwd prog args in
  result.stdout
;;

let cd dir = Unix.chdir dir
;;

let pwd () = Sys.getcwd ()
;;

let echo s = print_endline s
;;

let run_raw = run
;;

module Cmd = struct
  type 'a t = unit -> ('a, string) result

  let return value () = Ok value
  let fail msg () = Error msg

  let bind t f () =
    match t () with
    | Ok value -> f value ()
    | Error _ as err -> err

  let map t f () =
    match t () with
    | Ok value -> Ok (f value)
    | Error _ as err -> err

  let ( let* ) = bind
  let ( let+ ) t f = map t f

  let run t = t ()

  let run_cmd ?cwd cmd () =
    let result = run_raw ?cwd cmd in
    if result.status = 0 then Ok result else Error result.stderr

  let sh ?cwd cmd =
    let+ result = run_cmd ?cwd cmd in
    result.stdout

  let cmd ?cwd prog args =
    let parts = prog :: List.map Filename.quote args in
    sh ?cwd (String.concat " " parts)
end
;;
|}
