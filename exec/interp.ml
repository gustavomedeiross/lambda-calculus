let args () =
  match Array.to_list Sys.argv with
  | [_; filename] -> filename
  | _ ->
     print_endline "Usage: dune exec ./src/lambda.exe [filename]";
     exit 0

let read_all filename =
  let ch = open_in filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str

let () =
  let filename = args () in
  let contents = read_all filename in
  contents
  |> Lambda.interp
  |> Lambda.Eval.value_to_string
  |> print_endline
