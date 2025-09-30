open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module ErrorCode = struct
  type t =
    | ParseError
    | InvalidParams
    | InternalError
    | MethodNotFound
    | InvalidRequest
    | ServerError

  let to_int error =
    match error with
    | ParseError -> -32700
    | InvalidRequest -> -32600
    | MethodNotFound -> -32601
    | InvalidParams -> -32602
    | InternalError -> -32603
    | ServerError -> -32099
  ;;

  let from_int error =
    match error with
    | -32700 -> ParseError
    | -32600 -> InvalidRequest
    | -32601 -> MethodNotFound
    | -32602 -> InvalidParams
    | -32603 -> InternalError
    | -32099 -> ServerError
    | _ -> failwith "Not an Error Code"
  ;;

  let yojson_of_t t = t |> to_int |> yojson_of_int
end

type t =
  { code : ErrorCode.t
  ; message : string
  }
[@@deriving yojson_of]

let create (code : ErrorCode.t) (message : string) : t = { code; message }
