open Salt1

let () =
  if Array.length Sys.argv <> 2
  then print_endline "USAGE: dune exec salt1 <filename>"
  else
    let filename = Sys.argv.(1) in
    match Salt1.interp ~filename with
    | Ok (_, v) -> Format.printf "%a" Ast.pp_value v
    | Error e ->
      In_channel.with_open_text filename
        (fun ic ->
           let text = In_channel.input_all ic in
           let msg = Error_msg.to_string ~filename ~text e in
           Printf.eprintf "%s" msg)
