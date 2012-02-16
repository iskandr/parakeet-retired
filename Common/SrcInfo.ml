
type t = {
  filename: string option;
  line: int;
  col: int;
}

let empty = {
  line = 0;
  col = 0;
  filename=None
}

let mk_source_info ?filename line col = {
  line=line;
  col= col;
  filename=filename
}

let to_str src =
  Printf.sprintf "%sline %d, column %d%s"
    (match src.filename with None -> "" | Some str -> (str ^ " "))
    src.line
    src.col
    (match src.filename with None -> "" | Some str -> " in file " ^ str)