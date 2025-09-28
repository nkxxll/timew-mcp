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

type server_capabilities =
  { tools : server_tools
  ; resources : empty_obj
  }
[@@deriving yojson]

type server_initialize_result =
  { protocol_version : string [@key "protocolVersion"]
  ; capabilities : server_capabilities
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
[@@deriving yojson, yojson_fields, show]

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

type server_error_response =
  { jsonrpc : string
  ; id : int option
  ; error : Mcp_error.t
  }
[@@deriving yojson_of]
