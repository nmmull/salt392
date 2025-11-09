let () =
  let filename = Sys.argv.(1) in
  match Salt.Parse.parse ~filename with
  | Ok _ -> ()
  | Error e ->
    In_channel.with_open_text filename
      (fun ic ->
         let text = In_channel.input_all ic in
         let msg = Error_msg.to_string ~filename ~text e in
         Printf.eprintf "%s" msg)
