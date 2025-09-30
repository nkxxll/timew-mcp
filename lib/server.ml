open Mcp_types

type handlers =
  { on_initialize :
      string
      -> server_info:info
      -> capabilities:server_capabilities
      -> (string, Mcp_error.t) Result.t
  ; on_notification : string -> (string, Mcp_error.t) Result.t
  ; on_toollist : string -> (string, Mcp_error.t) Result.t
  ; on_toolcall : string -> (string, Mcp_error.t) Result.t
  }

type t =
  { handlers : handlers
  ; server_info : info
  ; server_capabilities : server_capabilities
  }

let create ~handlers ~server_info ~server_capabilities =
  { handlers; server_info; server_capabilities }
;;

let get_id message =
  let open Yojson.Safe.Util in
  message |> member "id" |> to_int
;;

let get_method message =
  let open Yojson.Safe.Util in
  try
    let method_str = message |> member "method" |> to_string in
    Method.from_string method_str
  with
  | _ -> Error "missing or invalid 'method' field"
;;

let handle_request (server : t) (request : string) =
  let json_request = Yojson.Safe.from_string request in
  match get_method json_request with
  | Error msg ->
    Error
      (Mcp_error.create
         MethodNotFound
         (Printf.sprintf "Method could not be found: %s." msg))
  | Ok method_ ->
    (match method_ with
     | NotificationsInitialized -> server.handlers.on_notification request
     | Initialize ->
       server.handlers.on_initialize
         request
         ~server_info:server.server_info
         ~capabilities:server.server_capabilities
     | ToolsList -> server.handlers.on_toollist request
     | ToolsCall -> server.handlers.on_toolcall request)
;;
