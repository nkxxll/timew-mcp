open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let rpc_version = "2.0"
let protocol_version = "2025-06-18"

module Method = struct
  type t =
    | Initialize
    | NotificationsInitialized
    | ToolsList
    | ToolsCall

  let to_string = function
    | Initialize -> "initialize"
    | NotificationsInitialized -> "notifications/initialized"
    | ToolsList -> "tools/list"
    | ToolsCall -> "tools/call"
  ;;

  let from_string method_string =
    match method_string with
    | "initialize" -> Ok Initialize
    | "notifications/initialized" -> Ok NotificationsInitialized
    | "tools/list" -> Ok ToolsList
    | "tools/call" -> Ok ToolsCall
    | _ -> Error ("unknown method: " ^ method_string)
  ;;
end

type empty_obj = unit

let empty_obj_of_yojson _empty_obj = ()
let yojson_of_empty_obj _empty_obj = `Assoc []

type info =
  { name : string
  ; version : string
  }
[@@deriving yojson]

type server_tools = { list_changed : bool [@key "listChanged"] }
[@@deriving yojson, yojson_fields]

type server_capabilies =
  { tools : server_tools
  ; resources : empty_obj
  }
[@@deriving yojson]

type server_initialize_result =
  { protocol_version : string [@key "protocolVersion"]
  ; capabilities : server_capabilies
  ; server_info : info [@key "serverInfo"]
  }
[@@deriving yojson, yojson_fields]

type summary_time =
  { type_ : string [@key "type"]
  ; enum : string list
  ; description : string
  ; default : string
  }
[@@deriving yojson, show]

let summary_time_schema =
  { type_ = "string"
  ; enum = [ "day"; "week" ]
  ; description = "Time frame the summary should span currently only day or week"
  ; default = "week"
  }
;;

type input_schema_properties = { summary_time : summary_time [@key "summaryTime"] }
[@@deriving yojson, yojson_fields, show]

type input_schema =
  { type_ : string [@key "type"]
  ; properties : input_schema_properties
  ; required : string list
  }
[@@deriving yojson, yojson_fields, show]

type tool =
  { name : string
  ; title : string
  ; description : string
  ; input_schema : input_schema [@key "inputSchema"]
  }
[@@deriving yojson, yojson_fields, show]

type server_tool_discovery_result = { tools : tool list } [@@deriving yojson, show]

type server_response =
  { jsonrpc : string
  ; id : int
  ; result : server_initialize_result
  }
[@@deriving yojson]

type params =
  { protocol_version : string [@key "protocolVersion"]
  ; capabilities : empty_obj
  ; client_info : info [@key "clientInfo"]
  }
[@@deriving yojson, yojson_fields]

type client_request =
  { jsonrpc : string
  ; id : int
  ; method_ : string [@key "method"]
  ; params : params
  }
[@@deriving yojson, yojson_fields]

type server_tool_discovery_response =
  { jsonrpc : string
  ; id : int
  ; result : server_tool_discovery_result
  }
[@@deriving yojson, show]

type summary_time_arguments = { summary_time : string [@key "summaryTime"] }
[@@deriving yojson, show]

type tool_call_params =
  { name : string
  ; arguments : summary_time_arguments
  }
[@@deriving yojson, show]

type client_tool_call_request =
  { jsonrpc : string
  ; id : int
  ; method_ : string [@key "method"]
  ; params : tool_call_params
  }
[@@deriving yojson, show]

type tool_call_content =
  { type_ : string [@key "type"]
  ; text : string
  }
[@@deriving yojson, show]

type tool_call_result = { content : tool_call_content list } [@@deriving yojson, show]

type server_tool_call_response =
  { jsonrpc : string
  ; id : int
  ; result : tool_call_result
  }
[@@deriving yojson, show]

type response_error =
  { code : int
  ; message : string
  }
[@@deriving yojson_of]

type server_error_response =
  { jsonrpc : string
  ; id : int option
  ; error : response_error
  }
[@@deriving yojson_of]

let handle_tool_call client_message =
  let json_client_request = Yojson.Safe.from_string client_message in
  let client_request = client_tool_call_request_of_yojson json_client_request in
  let tool_name = client_request.params.name in
  if String.equal tool_name "summaryTime"
  then (
    let args = client_request.params.arguments in
    let time_frame = args.summary_time in
    if not (String.equal time_frame "day" || String.equal time_frame "week")
    then
      Error
        { code = -32602 (* Invalid params *)
        ; message = "invalid argument for summaryTime: " ^ time_frame
        }
    else (
      let json_summary_res = Timew.get_summary () in
      match json_summary_res with
      | Ok json_summary ->
        let response_content =
          [ { type_ = "text"; text = "The summary returned this JSON: " ^ json_summary } ]
        in
        let server_response : server_tool_call_response =
          { jsonrpc = rpc_version
          ; id = client_request.id
          ; result = { content = response_content }
          }
        in
        Ok (yojson_of_server_tool_call_response server_response)
      | Error err ->
        Error
          { code = -32603; message = "Internal server error while retrieving the data" ^ err }))
  else (* Handle unknown tool *)
    Error { code = -32601 (* Method not found *); message = "Unknown tool: " ^ tool_name }
;;

let handle_initialize client_message ~server_info ~capabilities =
  let json_client_request = Yojson.Safe.from_string client_message in
  let client_request = client_request_of_yojson json_client_request in
  let server_response : server_response =
    { jsonrpc = rpc_version
    ; id = client_request.id
    ; result = { protocol_version; server_info; capabilities }
    }
  in
  yojson_of_server_response server_response
;;

let get_method message =
  let open Yojson.Safe.Util in
  try
    let method_str = message |> member "method" |> to_string in
    Method.from_string method_str
  with
  | _ -> Error "missing or invalid 'method' field"
;;

let get_id message =
  let open Yojson.Safe.Util in
  message |> member "id" |> to_int
;;

let handle_tools_list message =
  let id = get_id message in
  let summary_tool =
    { name = "summaryTime"
    ; title = "Time summary"
    ; description = "Get a summary of the tracked time for a given period"
    ; input_schema =
        { type_ = "object"
        ; properties = { summary_time = summary_time_schema }
        ; required = [ "summaryTime" ]
        }
    }
  in
  let response : server_tool_discovery_response =
    { jsonrpc = rpc_version; id; result = { tools = [ summary_tool ] } }
  in
  yojson_of_server_tool_discovery_response response
;;

let handle_message message ~server_info ~capabilities =
  let id =
    try Some (get_id message) with
    | _ -> None
  in
  let create_error_response code message =
    let error_response : server_error_response =
      { jsonrpc = rpc_version; id; error = { code; message } }
    in
    yojson_of_server_error_response error_response |> Yojson.Safe.to_string
  in
  match get_method message with
  | Error msg -> Some (create_error_response (-32601) msg) (* Method not found *)
  | Ok method_ ->
    (match method_ with
     | NotificationsInitialized -> None
     | Initialize ->
       let client_message_string = Yojson.Safe.to_string message in
       let response =
         handle_initialize client_message_string ~server_info ~capabilities
       in
       Some (Yojson.Safe.to_string response)
     | ToolsList ->
       let response = handle_tools_list message in
       Some (Yojson.Safe.to_string response)
     | ToolsCall ->
       let client_message_string = Yojson.Safe.to_string message in
       (match handle_tool_call client_message_string with
        | Ok response -> Some (Yojson.Safe.to_string response)
        | Error err -> Some (create_error_response err.code err.message)))
;;

let%expect_test "get method" =
  let messages =
    [ {|{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": {
      "name": "example-client",
      "version": "1.0.0"
    }
  }
  }|}
    ; {|{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
  }|}
    ; {|{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list"
  }|}
    ; {|{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "weather_current",
    "arguments": {
      "location": "San Francisco",
      "units": "imperial"
    }
  }
    }|}
    ; {|{ "jsonrpc": "2.0", "id": 4, "method": "invalid" }|}
    ; {|{ "jsonrpc": "2.0", "id": 5 }|}
    ]
  in
  List.iter messages ~f:(fun msg ->
    let json = Yojson.Safe.from_string msg in
    match get_method json with
    | Ok m -> Method.to_string m |> Stdio.print_endline
    | Error e -> Stdio.print_endline e);
  [%expect
    {|
    initialize
    notifications/initialized
    tools/list
    tools/call
    unknown method: invalid
    missing or invalid 'method' field
    |}]
;;

let%expect_test "initialize message" =
  let server_info = { name = "timew-mcp"; version = "1.0.0" } in
  let capabilities = { tools = { list_changed = true }; resources = () } in
  let message =
    {|{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": {
      "name": "example-client",
      "version": "1.0.0"
    }
  }
    }|}
  in
  handle_initialize message ~capabilities ~server_info
  |> Yojson.Safe.pretty_to_string
  |> Stdio.print_endline;
  [%expect
    {|
    {
      "jsonrpc": "2.0",
      "id": 1,
      "result": {
        "protocolVersion": "2025-06-18",
        "capabilities": { "tools": { "listChanged": true }, "resources": {} },
        "serverInfo": { "name": "timew-mcp", "version": "1.0.0" }
      }
    }
    |}]
;;

let%expect_test "parse tool" =
  let message =
    {|{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name": "calculator_arithmetic",
        "title": "Calculator",
        "description": "Perform mathematical calculations including basic arithmetic, trigonometric functions, and algebraic operations",
        "inputSchema": {
          "type": "object",
          "properties": {
            "summaryTime": {
              "type": "string",
              "enum": ["day", "week"],
              "description": "this is a desc",
              "default": "week"
            }
          },
          "required": ["summaryTime"]
        }
      },
      {
        "name": "weather_current",
        "title": "Weather Information",
        "description": "Get current weather information for any location worldwide",
        "inputSchema": {
          "type": "object",
          "properties": {
            "summaryTime": {
              "type": "string",
              "enum": ["day", "week"],
              "description": "some things that should be here",
              "default": "week"
            }
          },
          "required": ["summaryTime"]
        }
      }
    ]
  }
}
|}
  in
  let json = Yojson.Safe.from_string message in
  let server_res = server_tool_discovery_response_of_yojson json in
  show_server_tool_discovery_response server_res |> Stdio.print_endline;
  [%expect
    {|
    { Timew_mcp.jsonrpc = "2.0"; id = 2;
      result =
      { Timew_mcp.tools =
        [{ Timew_mcp.name = "calculator_arithmetic"; title = "Calculator";
           description =
           "Perform mathematical calculations including basic arithmetic, trigonometric functions, and algebraic operations";
           input_schema =
           { Timew_mcp.type_ = "object";
             properties =
             { Timew_mcp.summary_time =
               { Timew_mcp.type_ = "string"; enum = ["day"; "week"];
                 description = "this is a desc"; default = "week" }
               };
             required = ["summaryTime"] }
           };
          { Timew_mcp.name = "weather_current"; title = "Weather Information";
            description =
            "Get current weather information for any location worldwide";
            input_schema =
            { Timew_mcp.type_ = "object";
              properties =
              { Timew_mcp.summary_time =
                { Timew_mcp.type_ = "string"; enum = ["day"; "week"];
                  description = "some things that should be here";
                  default = "week" }
                };
              required = ["summaryTime"] }
            }
          ]
        }
      }
    |}]
;;

let%expect_test "handle message" =
  let server_info = { name = "timew-mcp"; version = "1.0.0" } in
  let capabilities = { tools = { list_changed = true }; resources = () } in
  let messages =
    [ {|{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": {
      "name": "example-client",
      "version": "1.0.0"
    }
  }
    }|}
    ; {|{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
    }|}
    ; {|{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list"
    }|}
    ; {|{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "summaryTime",
    "arguments": {
      "summaryTime": "week"
    }
  }
    }|}
    ; {|{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "summaryTime",
    "arguments": {
      "summaryTime": "day"
    }
  }
    }|}
    ; {|{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "invalidMethod"
    }|}
    ]
  in
  List.iter messages ~f:(fun msg_string ->
    let message = Yojson.Safe.from_string msg_string in
    let response = handle_message message ~server_info ~capabilities in
    match response with
    | Some response_json_string ->
      Yojson.Safe.from_string response_json_string
      |> Yojson.Safe.pretty_to_string
      |> Stdio.print_endline
    | None -> Stdio.print_endline "Notification, no response");
  [%expect
    {|
    {
      "jsonrpc": "2.0",
      "id": 1,
      "result": {
        "protocolVersion": "2025-06-18",
        "capabilities": { "tools": { "listChanged": true }, "resources": {} },
        "serverInfo": { "name": "timew-mcp", "version": "1.0.0" }
      }
    }
    Notification, no response
    {
      "jsonrpc": "2.0",
      "id": 2,
      "result": {
        "tools": [
          {
            "name": "summaryTime",
            "title": "Time summary",
            "description": "Get a summary of the tracked time for a given period",
            "inputSchema": {
              "type": "object",
              "properties": {
                "summaryTime": {
                  "type": "string",
                  "enum": [ "day", "week" ],
                  "description": "Time frame the summary should span currently only day or week",
                  "default": "week"
                }
              },
              "required": [ "summaryTime" ]
            }
          }
        ]
      }
    }
    {
      "jsonrpc": "2.0",
      "id": 3,
      "result": {
        "content": [
          { "type": "text", "text": "summary for the week will be implemented" }
        ]
      }
    }
    {
      "jsonrpc": "2.0",
      "id": 4,
      "result": {
        "content": [
          { "type": "text", "text": "summary for the day will be implemented" }
        ]
      }
    }
    {
      "jsonrpc": "2.0",
      "id": 6,
      "error": { "code": -32601, "message": "unknown method: invalidMethod" }
    }
    |}]
;;

let%expect_test "failed initialize" =
  let server_info = { name = "timew-mcp"; version = "1.0.0" } in
  let capabilities = { tools = { list_changed = true }; resources = () } in
  let message =
    {|{"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"claude-ai","version":"0.1.0"}},"jsonrpc":"2.0","id":0}|}
  in
  message
  |> Yojson.Safe.from_string
  |> handle_message ~capabilities ~server_info
  |> function
  | Some m -> Stdio.print_endline m
  | None ->
    Stdio.print_endline "error no message";
    [%expect
      {| {"jsonrpc":"2.0","id":0,"result":{"protocolVersion":"2025-06-18","capabilities":{"tools":{"listChanged":true},"resources":{}},"serverInfo":{"name":"timew-mcp","version":"1.0.0"}}} |}]
;;
