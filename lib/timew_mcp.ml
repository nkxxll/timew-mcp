open Types
open Core

let handle_tool_call client_message ~tool_function =
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
        { Mcp_error.code = InvalidParams (* Invalid params *)
        ; message = "invalid argument for summaryTime: " ^ time_frame
        }
    else (
      let json_summary_res = tool_function () in
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
          { Mcp_error.code = ServerError
          ; message = "Internal server error while retrieving the data" ^ err
          }))
  else Error { Mcp_error.code = MethodNotFound; message = "Unknown tool: " ^ tool_name }
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

let handle_tools_list client_message =
  let message = Yojson.Safe.from_string client_message in
  let id = Server.get_id message in
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

let on_initialize_handler client_message ~server_info ~capabilities =
  try Ok (handle_initialize client_message ~server_info ~capabilities |> Yojson.Safe.to_string) with
  | exn ->
    Error
      (Mcp_error.create
         ParseError
         ("Failed to parse initialize request: " ^ Exn.to_string exn))
;;

let on_notification_handler (_ : string) = Ok ""

let on_toollist_handler client_message =
  try Ok (handle_tools_list client_message |> Yojson.Safe.to_string) with
  | exn ->
    Error
      (Mcp_error.create
         ParseError
         ("Failed to parse tools/list request: " ^ Exn.to_string exn))
;;

let on_toolcall_handler ~tool_function client_message =
  try
    match handle_tool_call client_message ~tool_function with
    | Ok response_yojson -> Ok (Yojson.Safe.to_string response_yojson)
    | Error e -> Error e
  with
  | exn ->
    Error
      (Mcp_error.create
         ParseError
         ("Failed to parse tool/call request: " ^ Exn.to_string exn))
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
    match Server.get_method json with
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
    { Timew_mcp.Types.jsonrpc = "2.0"; id = 2;
      result =
      { Timew_mcp.Types.tools =
        [{ Timew_mcp.Types.name = "calculator_arithmetic";
           title = "Calculator";
           description =
           "Perform mathematical calculations including basic arithmetic, trigonometric functions, and algebraic operations";
           input_schema =
           { Timew_mcp.Types.type_ = "object";
             properties =
             { Timew_mcp.Types.summary_time =
               { Timew_mcp.Types.type_ = "string"; enum = ["day"; "week"];
                 description = "this is a desc"; default = "week" }
               };
             required = ["summaryTime"] }
           };
          { Timew_mcp.Types.name = "weather_current";
            title = "Weather Information";
            description =
            "Get current weather information for any location worldwide";
            input_schema =
            { Timew_mcp.Types.type_ = "object";
              properties =
              { Timew_mcp.Types.summary_time =
                { Timew_mcp.Types.type_ = "string"; enum = ["day"; "week"];
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

let%expect_test "server handle_request" =
  let server_info = { name = "timew-mcp"; version = "1.0.0" } in
  let capabilities = { tools = { list_changed = true }; resources = () } in
  let tool_function () = Ok "test tool response" in
  let handlers : Server.handlers =
    { on_initialize = on_initialize_handler
    ; on_notification = on_notification_handler
    ; on_toollist = on_toollist_handler
    ; on_toolcall = on_toolcall_handler ~tool_function
    }
  in
  let server = Server.create ~handlers ~server_info ~server_capabilities:capabilities in
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
    match Server.handle_request server msg_string with
    | Ok response_json_string ->
      if response_json_string = ""
      then Stdio.print_endline "Notification, no response"
      else
        Yojson.Safe.from_string response_json_string
        |> Yojson.Safe.pretty_to_string
        |> Stdio.print_endline
    | Error err ->
      let id =
        try Some (Yojson.Safe.from_string msg_string |> Server.get_id) with
        | _ -> None
      in
      let error_response : server_error_response =
        { jsonrpc = rpc_version; id; error = err }
      in
      yojson_of_server_error_response error_response
      |> Yojson.Safe.pretty_to_string
      |> Stdio.print_endline);
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
          {
            "type": "text",
            "text": "The summary returned this JSON: test tool response"
          }
        ]
      }
    }
    {
      "jsonrpc": "2.0",
      "id": 4,
      "result": {
        "content": [
          {
            "type": "text",
            "text": "The summary returned this JSON: test tool response"
          }
        ]
      }
    }
    {
      "jsonrpc": "2.0",
      "id": 6,
      "error": {
        "code": -32601,
        "message": "Method could not be found: unknown method: invalidMethod."
      }
    }
    |}]
;;