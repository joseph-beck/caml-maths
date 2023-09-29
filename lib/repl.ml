open Stdio
open Token

let print_prompt () =
  printf "|> ";
  Out_channel.flush stdout

let rec repl () =
  print_prompt ();
  let input = In_channel.input_line stdin in
  match input with
  | None -> printf "error reading line"
  | Some str ->
      let tokens = get_tokens str in
      print_tokens tokens;
      repl ()
