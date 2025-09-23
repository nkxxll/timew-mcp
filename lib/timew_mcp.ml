open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Composition

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
    | "initialize" -> Initialize
    | "notifications/initialized" -> NotificationsInitialized
    | "tools/list" -> ToolsList
    | "tools/call" -> ToolsCall
    | _ -> failwith "method unknown"
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

type capabilities = { elicitation : empty_obj } [@@deriving yojson]

type params =
  { protocol_version : string [@key "protocolVersion"]
  ; capabilities : capabilities
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

let handle_tool_call client_message =
  let json_client_request = Yojson.Safe.from_string client_message in
  let client_request = client_tool_call_request_of_yojson json_client_request in
  let tool_name = client_request.params.name in
  if String.equal tool_name "summaryTime"
  then (
    let args = client_request.params.arguments in
    let time_frame = args.summary_time in
    if not (String.equal time_frame "day" || String.equal time_frame "week")
    then failwith ("invalid argument for summaryTime: " ^ time_frame)
    else (
      let response_content =
        [ { type_ = "text"
          ; text = Printf.sprintf "summary for the %s will be implemented" time_frame
          }
        ]
      in
      let server_response : server_tool_call_response =
        { jsonrpc = rpc_version
        ; id = client_request.id
        ; result = { content = response_content }
        }
      in
      yojson_of_server_tool_call_response server_response))
  else (* Handle unknown tool *)
    failwith ("Unknown tool: " ^ tool_name)
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
  message |> member "method" |> to_string |> Method.from_string
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
  let method_ = get_method message in
  match method_ with
  | Initialize ->
    let client_message_string = Yojson.Safe.to_string message in
    Some (handle_initialize client_message_string ~server_info ~capabilities)
  | ToolsCall ->
    let client_message_string = Yojson.Safe.to_string message in
    Some (handle_tool_call client_message_string)
  | ToolsList -> Some (handle_tools_list message)
  | NotificationsInitialized -> None
;;

let%expect_test "get method" =
  let messages =
    [ {|{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {
      "elicitation": {}
    },
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
    ]
  in
  List.iter
    messages
    ~f:(Yojson.Safe.from_string >> get_method >> Method.to_string >> Stdio.print_endline);
  [%expect
    {|
    initialize
    notifications/initialized
    tools/list
    tools/call
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
    "capabilities": {
      "elicitation": {}
    },
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

let%expect_test "tool call message" =
  let message =
    {|{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "summaryTime",
    "arguments": {
      "summaryTime": "day"
    }
  }
    }|}
  in
  handle_tool_call message |> Yojson.Safe.pretty_to_string |> Stdio.print_endline;
  [%expect
    {|
    {
      "jsonrpc": "2.0",
      "id": 3,
      "result": {
        "content": [
          { "type": "text", "text": "summary for the day will be implemented" }
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
    "capabilities": {
      "elicitation": {}
    },
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
    ]
  in
  List.iter messages ~f:(fun msg_string ->
    let message = Yojson.Safe.from_string msg_string in
    let response = handle_message message ~server_info ~capabilities in
    match response with
    | Some response_json ->
      response_json |> Yojson.Safe.pretty_to_string |> Stdio.print_endline
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
    |}]
;;

