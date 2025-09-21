module Map = Map.Make (String)

type pos = Lexing.position * Lexing.position
let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos

let ( let* ) = Result.bind
let guard b e = if b then Ok () else Error e
