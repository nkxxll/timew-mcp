open Core

let () = Timew.get_summary () |> Result.iter ~f:(fun res -> Stdio.print_endline res)
