open Salt

let usage =  "USAGE: dune exec -- salt <version> <filename>"

let () =
  if Array.length Sys.argv <> 3
  then print_endline usage
  else
    let version = Sys.argv.(1) in
    match int_of_string_opt version with
    | Some version when version >= 0 && version <= 3 -> (
      let filename = Sys.argv.(2) in
      let interp =
        match version with
        | 0 -> Salt0.interp
        | 1 -> Salt1.interp
        | 2 -> Salt2.interp
        | 3 -> Salt3.interp
        | _ -> assert false
      in
      match interp ~filename with
      | Ok () -> ()
      | Error e ->
        In_channel.with_open_text filename
          (fun ic ->
             let text = In_channel.input_all ic in
             let msg = Error_msg.to_string ~filename ~text e in
             Printf.eprintf "%s" msg)
    )
    | _ -> print_endline usage
