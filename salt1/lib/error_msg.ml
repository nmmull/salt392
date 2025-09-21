open Utils

type t =
  {
    line_num: int;
    col_num: int;
    underline_width: int;
    filename: string;
    message: string;
    dummy: bool;
  }

let get_line text i =
  text
  |> String.split_on_char '\n'
  |> Fun.flip List.nth (i - 1)

let mk (pos : pos) message =
  let lloc, rloc = pos in
  let line_num = lloc.pos_lnum in
  let col_num = lloc.pos_cnum - lloc.pos_bol in
  let underline_width =
    if lloc.pos_lnum = lloc.pos_lnum
    then (rloc.pos_cnum - rloc.pos_bol) - col_num
    else 1
  in
  let filename = lloc.pos_fname in
  {
    line_num;
    col_num;
    underline_width;
    filename;
    message;
    dummy = pos = dummy_pos;
  }

let to_string ?filename ?text t =
  let err_msg_first_line = Printf.sprintf "error: %s" t.message in
  if t.dummy
  then err_msg_first_line
  else
    let filename = Option.value filename ~default:t.filename in
    let line_num_width = String.length (string_of_int t.line_num) in
    let err_msg_second_line =
      Printf.sprintf
        "%s--> %s:%d:%d"
        (String.make line_num_width ' ')
        filename
        t.line_num
        (t.col_num + 1)
    in
    let err_header =
      String.concat "\n"
        [
          err_msg_first_line;
          err_msg_second_line;
        ]
    in
    match text with
    | None -> err_header
    | Some text ->
      let line = get_line text t.line_num in
      let err_msg_third_line = String.make (line_num_width + 1) ' ' ^ "|" in
      let err_msg_fourth_line = Printf.sprintf "%d | %s" t.line_num line in
      let err_msg_fifth_line =
        Printf.sprintf
          "%s %s%s %s"
          err_msg_third_line
          (String.make t.col_num ' ')
          (String.make t.underline_width '^')
          t.message
      in
      String.concat "\n"
        [
          err_header;
          err_msg_third_line;
          err_msg_fourth_line;
          err_msg_fifth_line;
        ]

let dummy = mk dummy_pos "dummy"
let missing_main = mk dummy_pos "`main` function found"

let exp_ty t1 t2 pos =
  let msg =
    let open Ast in
    Format.asprintf
      "expected %a, got %a"
      pp_type t1 pp_type t2
  in mk pos msg

let unknown_var x pos =
  mk pos (Format.asprintf "unknown variable `%s`" x)

let not_implemented pos = mk pos "not implemented"

let assert_fail pos = mk pos "failed assertion"

let cannot_deref pos ty =
  mk pos (Format.asprintf "type `%a` cannot be dereferenced" Ast.pp_type ty)

let cannot_assign pos x =
  mk pos (Format.asprintf "cannot assign twice to immutable variable `%s`" x)

let invalid_assign pos =
  mk pos "invalid left-hand side of assignment"
