(* this should not be *)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Core

module ExportArguments = struct
  type t =
    | Day
    | Week

  let to_string = function
    | Day -> ":day"
    | Week -> ":week"
  ;;

  let from_string = function
    | "day" -> Day
    | "week" -> Week
    | ":day" -> Day
    | ":week" -> Week
    | s -> failwith (Printf.sprintf "invalid argument %s" s)
  ;;
end

module TimewCommand = struct
  type t =
    { command : string
    ; args : ExportArguments.t option
    }

  let base_command = "timew"
  let create command args = { command; args }

  let exec t =
    let ic =
      match t.args with
      | Some a ->
        let sa = ExportArguments.to_string a in
        Core_unix.open_process_in (String.concat ~sep:" " [ base_command; t.command; sa ])
      | None ->
        Core_unix.open_process_in (String.concat ~sep:" " [ base_command; t.command ])
    in
    let outstring =
      "The json response from the timew export is: \n" ^ In_channel.input_all ic
    in
    match Core_unix.close_process_in ic with
    | Error (`Exit_non_zero n) ->
      Error (Format.sprintf "timew export failed with exit code %d\n" n)
    | Error (`Signal n) ->
      Error
        (Format.sprintf "timew export failed with exit code %s\n" (Signal.to_string n))
    | Ok _ -> Ok outstring
  ;;
end

type summary =
  { id : int
  ; start : string
  ; end_ : string option [@key "end"] [@yojson.option]
  ; tags : string list
  }
[@@deriving show, yojson, yojson_fields]

type summary_list = summary list [@@deriving yojson, show]

let%expect_test "read summary" =
  let input =
    {|[
{"id":5,"start":"20250922T060220Z","end":"20250922T060237Z","tags":["PROGRAMMING"]},
{"id":4,"start":"20250922T063459Z","end":"20250922T065409Z","tags":["THESIS"]},
{"id":3,"start":"20250922T080139Z","end":"20250922T105638Z","tags":["UNI"]},
{"id":2,"start":"20250922T124626Z","end":"20250922T143000Z","tags":["THESIS"]},
{"id":1,"start":"20250922T165210Z","tags":["THESIS"]}
  ]|}
  in
  let json = input |> Yojson.Safe.from_string |> summary_list_of_yojson in
  json |> show_summary_list |> Stdio.print_endline;
  [%expect
    {|
    [{ Timew.id = 5; start = "20250922T060220Z";
       end_ = (Some "20250922T060237Z"); tags = ["PROGRAMMING"] };
      { Timew.id = 4; start = "20250922T063459Z";
        end_ = (Some "20250922T065409Z"); tags = ["THESIS"] };
      { Timew.id = 3; start = "20250922T080139Z";
        end_ = (Some "20250922T105638Z"); tags = ["UNI"] };
      { Timew.id = 2; start = "20250922T124626Z";
        end_ = (Some "20250922T143000Z"); tags = ["THESIS"] };
      { Timew.id = 1; start = "20250922T165210Z"; end_ = None; tags = ["THESIS"]
        }
      ]
    |}]
;;
