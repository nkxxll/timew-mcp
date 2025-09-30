open Core
open Timew_mcp
open Mcp_types

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
  let tool_function () = Timew.get_summary () in
  let handlers : Server.handlers =
    { on_initialize = on_initialize_handler
    ; on_notification = on_notification_handler
    ; on_toollist = on_toollist_handler
    ; on_toolcall = on_toolcall_handler ~tool_function
    }
  in
  let server = Server.create ~handlers ~server_info ~server_capabilities:capabilities in
  let my_handler (msg : string) : string option =
    try
      match Server.handle_request server msg with
      | Ok response_str ->
        if String.equal "" response_str then None else Some response_str
      | Error err ->
        let id =
          try Some (Yojson.Safe.from_string msg |> Server.get_id) with
          | _ -> None
        in
        let error_response : server_error_response =
          { jsonrpc = rpc_version; id; error = err }
        in
        Some (yojson_of_server_error_response error_response |> Yojson.Safe.to_string)
    with
    | Yojson.Json_error err ->
      let error_response : server_error_response =
        { jsonrpc = rpc_version
        ; id = None
        ; error =
            { code = Mcp_error.ErrorCode.ParseError (* Parse error *)
            ; message = "Failed to parse message: " ^ err
            }
        }
      in
      Some (yojson_of_server_error_response error_response |> Yojson.Safe.to_string)
    | exn ->
      let id =
        try Some (Yojson.Safe.from_string msg |> Server.get_id) with
        | _ -> None
      in
      let error_response : server_error_response =
        { jsonrpc = rpc_version
        ; id
        ; error =
            { code = Mcp_error.ErrorCode.InternalError (* Internal error *)
            ; message = "Internal error: " ^ Exn.to_string exn
            }
        }
      in
      Some (yojson_of_server_error_response error_response |> Yojson.Safe.to_string)
  in
  serve_loop ~handler:my_handler
;;

