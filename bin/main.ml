open Core
open Timew_mcp

let rec serve_loop ~handler =
  match In_channel.input_line In_channel.stdin with
  | Some line ->
    (match handler line with
     | Some response ->
       Out_channel.output_line Out_channel.stdout response;
       Out_channel.flush Out_channel.stdout
     | None -> ());
    serve_loop ~handler
  | None -> ()
;;

let () =
  (* configure reporter: log to stderr with nice formatting *)
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  (* show Info and above *)
  let server_info = { name = "timew-mcp"; version = "1.0.0" } in
  let capabilities = { tools = { list_changed = true }; resources = () } in
  let my_handler (msg : string) : string option =
    try
      let message = Yojson.Safe.from_string msg in
      handle_message message ~server_info ~capabilities ~tool_function:Timew.get_summary
    with
    | Yojson.Json_error err ->
      let error_response : server_error_response =
        { jsonrpc = rpc_version
        ; id = None
        ; error =
            { code = -32700 (* Parse error *)
            ; message = "Failed to parse message: " ^ err
            }
        }
      in
      Some (yojson_of_server_error_response error_response |> Yojson.Safe.to_string)
    | exn ->
      let id =
        try Some (Yojson.Safe.from_string msg |> get_id) with
        | _ -> None
      in
      let error_response : server_error_response =
        { jsonrpc = rpc_version
        ; id
        ; error =
            { code = -32603 (* Internal error *)
            ; message = "Internal error: " ^ Exn.to_string exn
            }
        }
      in
      Some (yojson_of_server_error_response error_response |> Yojson.Safe.to_string)
  in
  serve_loop ~handler:my_handler
;;

