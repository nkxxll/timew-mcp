open Lwt.Syntax
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* MCP Protocol Version *)
let mcp_version = "2024-11-05"

(* Server capabilities *)
type server_capabilities =
  { logging : logging_capability option [@yojson.option]
  ; prompts : prompts_capability option [@yojson.option]
  ; resources : resources_capability option [@yojson.option]
  ; tools : tools_capability option [@yojson.option]
  }
[@@deriving yojson_of]

and logging_capability = unit [@@deriving yojson_of]

and prompts_capability =
  { list_changed : bool option [@yojson.option] [@yojson.key "listChanged"] }
[@@deriving yojson_of]

and resources_capability =
  { subscribe : bool option [@yojson.option]
  ; list_changed : bool option [@yojson.option] [@yojson.key "listChanged"]
  }
[@@deriving yojson_of]

and tools_capability =
  { list_changed : bool option [@yojson.option] [@yojson.key "listChanged"] }
[@@deriving yojson_of]

(* Server information *)
type server_info =
  { name : string
  ; version : string
  }
[@@deriving yojson_of]

(* Client capabilities *)
type client_capabilities =
  { experimental : string list option [@yojson.option]
  ; sampling : sampling_capability option [@yojson.option]
  }
[@@deriving of_yojson]

and sampling_capability = unit [@@deriving of_yojson]

(* Client info *)
type client_info =
  { name : string
  ; version : string
  }
[@@deriving of_yojson]

(* Initialize request from client *)
type initialize_request =
  { protocol_version : string [@yojson.key "protocolVersion"]
  ; capabilities : client_capabilities
  ; client_info : client_info [@yojson.key "clientInfo"]
  }
[@@deriving of_yojson]

(* Initialize response from server *)
type initialize_response =
  { protocol_version : string [@yojson.key "protocolVersion"]
  ; capabilities : server_capabilities
  ; server_info : server_info [@yojson.key "serverInfo"]
  }
[@@deriving yojson_of]

(* JSON-RPC error codes *)
let invalid_request_code = -32600
let method_not_found_code = -32601
let invalid_params_code = -32602
let internal_error_code = -32603

(* JSON-RPC message types *)
type jsonrpc_request =
  { jsonrpc : string
  ; id : int option [@yojson.option]
  ; method_name : string [@yojson.key "method"]
  ; params : string list option [@yojson.option]
  }
[@@deriving of_yojson]

type jsonrpc_error =
  { code : int
  ; message : string
  ; data : string option [@yojson.option]
  }
[@@deriving yojson_of]

type jsonrpc_response =
  { jsonrpc : string
  ; id : Yojson.Safe.t option [@yojson.option]
  ; result : Yojson.Safe.t option [@yojson.option]
  ; error : jsonrpc_error option [@yojson.option]
  }
[@@deriving yojson_of]

(* Create server capabilities based on what your server supports *)
let create_server_capabilities () =
  { logging = Some ()
  ; prompts = Some { list_changed = Some true }
  ; resources = Some { subscribe = Some false; list_changed = Some true }
  ; tools = Some { list_changed = Some true }
  }
;;

(* Handle the initialize request *)
let handle_initialize_request req =
  (* Validate protocol version *)
  if req.protocol_version <> mcp_version
  then
    Error
      ( invalid_params_code
      , Printf.sprintf "Unsupported protocol version: %s" req.protocol_version )
  else (
    let response =
      { protocol_version = mcp_version
      ; capabilities = create_server_capabilities ()
      ; server_info = { name = "my-mcp-server"; version = "1.0.0" }
      }
    in
    Ok response)
;;

let create_jsonrpc_response ?result ?error id =
  let error_obj =
    match error with
    | Some (code, message) -> Some { code; message; data = None }
    | None -> None
  in
  { jsonrpc = "2.0"; id; result; error = error_obj }
;;

(* Parse JSON-RPC request with better error handling *)
let parse_jsonrpc_request json =
  try Ok (jsonrpc_request_of_yojson json) with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
    Error (Printf.sprintf "Invalid JSON-RPC request: %s" (Printexc.to_string exn))
  | exn ->
    Error (Printf.sprintf "Failed to parse JSON-RPC request: %s" (Printexc.to_string exn))
;;

(* Parse initialize request with better error handling *)
let parse_initialize_request json =
  try Ok (initialize_request_of_yojson json) with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
    Error (Printf.sprintf "Invalid initialize request: %s" (Printexc.to_string exn))
  | exn ->
    Error
      (Printf.sprintf "Failed to parse initialize request: %s" (Printexc.to_string exn))
;;

(* Main request handler *)
let handle_request_string request_str =
  let* () = Lwt.return_unit in
  try
    let json = Yojson.Safe.from_string request_str in
    match parse_jsonrpc_request json with
    | Error msg ->
      let error_response =
        create_jsonrpc_response ~error:(invalid_request_code, msg) None
      in
      let response_json = yojson_of_jsonrpc_response error_response in
      Lwt.return (Yojson.Safe.pretty_to_string response_json)
    | Ok jsonrpc_req ->
      (match jsonrpc_req.method_name with
       | "initialize" ->
         (match jsonrpc_req.params with
          | Some params ->
            (match parse_initialize_request params with
             | Error msg ->
               let error_response =
                 create_jsonrpc_response ~error:(invalid_params_code, msg) jsonrpc_req.id
               in
               let response_json = yojson_of_jsonrpc_response error_response in
               Lwt.return (Yojson.Safe.pretty_to_string response_json)
             | Ok init_req ->
               (match handle_initialize_request init_req with
                | Error (code, msg) ->
                  let error_response =
                    create_jsonrpc_response ~error:(code, msg) jsonrpc_req.id
                  in
                  let response_json = yojson_of_jsonrpc_response error_response in
                  Lwt.return (Yojson.Safe.pretty_to_string response_json)
                | Ok init_resp ->
                  let result = yojson_of_initialize_response init_resp in
                  let success_response = create_jsonrpc_response ~result jsonrpc_req.id in
                  let response_json = yojson_of_jsonrpc_response success_response in
                  Lwt.return (Yojson.Safe.pretty_to_string response_json)))
          | None ->
            let error_response =
              create_jsonrpc_response
                ~error:(invalid_params_code, "Missing params for initialize")
                jsonrpc_req.id
            in
            let response_json = yojson_of_jsonrpc_response error_response in
            Lwt.return (Yojson.Safe.pretty_to_string response_json))
       | method_name ->
         let error_response =
           create_jsonrpc_response
             ~error:
               (method_not_found_code, Printf.sprintf "Method not found: %s" method_name)
             jsonrpc_req.id
         in
         let response_json = yojson_of_jsonrpc_response error_response in
         Lwt.return (Yojson.Safe.string_of_json response_json))
  with
  | Yojson.Json_error msg ->
    let error_response =
      create_jsonrpc_response
        ~error:(invalid_request_code, Printf.sprintf "Invalid JSON: %s" msg)
        None
    in
    let response_json = yojson_of_jsonrpc_response error_response in
    Lwt.return (Yojson.Safe.pretty_to_string response_json)
  | exn ->
    let error_response =
      create_jsonrpc_response
        ~error:
          ( internal_error_code
          , Printf.sprintf "Internal error: %s" (Printexc.to_string exn) )
        None
    in
    let response_json = yojson_of_jsonrpc_response error_response in
    Lwt.return (Yojson.Safe.pretty_to_string response_json)
;;

(* Helper function to handle notifications (requests without id) *)
let handle_notification method_name params =
  match method_name with
  | "initialized" ->
    (* Client signals that initialization is complete *)
    Printf.printf "Client initialization complete\n%!";
    Lwt.return_unit
  | _ ->
    Printf.printf "Unknown notification: %s\n%!" method_name;
    Lwt.return_unit
;;

(* Extended handler that supports both requests and notifications *)
let handle_message_string message_str =
  let* () = Lwt.return_unit in
  try
    let json = Yojson.Safe.from_string message_str in
    match parse_jsonrpc_request json with
    | Error _ ->
      (* If it's not a valid request, it might be a notification *)
      Lwt.return None
    | Ok jsonrpc_req ->
      (match jsonrpc_req.id with
       | None ->
         (* This is a notification *)
         let* () = handle_notification jsonrpc_req.method_name jsonrpc_req.params in
         Lwt.return None
       | Some _ ->
         (* This is a request that expects a response *)
         let* response_str = handle_request_string message_str in
         Lwt.return (Some response_str))
  with
  | exn ->
    Printf.printf "Error handling message: %s\n%!" (Printexc.to_string exn);
    Lwt.return None
;;

(* Example usage *)
let example_initialize_request =
  {|
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "experimental": {},
      "sampling": {}
    },
    "clientInfo": {
      "name": "example-client",
      "version": "1.0.0"
    }
  }
}
|}
;;

let example_initialized_notification =
  {|
{
  "jsonrpc": "2.0",
  "method": "initialized",
  "params": {}
}
|}
;;

let test_handshake () =
  (* Test initialize request *)
  let* response = handle_request_string example_initialize_request in
  Printf.printf "Server response to initialize:\n%s\n\n" response;
  (* Test initialized notification *)
  let* _ = handle_message_string example_initialized_notification in
  Printf.printf "Handled initialized notification\n";
  Lwt.return_unit
;;

(* Example dune file content you'll need:
   (executable
    (public_name mcp-server)
    (name main)
    (libraries lwt yojson ppx_yojson_conv)
    (preprocess (pps ppx_yojson_conv)))
*)
