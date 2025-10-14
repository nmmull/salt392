
module Map = Map.Make (String)

type pos = Lexing.position * Lexing.position
let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos
let dummy = Error_msg.mk dummy_pos "dummy"

let ( let* ) = Result.bind
let guard b e = if b then Ok () else Error e

let all_ok l =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | Ok x :: l ->
      go (x :: acc) l
    | Error e :: _ -> Error e
  in go [] l
