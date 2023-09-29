open Base
open Stdio

type token =
  | Token_number of int
  | Token_lbracket
  | Token_rbracket
  | Token_plus
  | Token_minus
  | Token_multiply
  | Token_divide

let get_tokens str =
  let rec next_token tokens = function
    | [] -> tokens
    | '(' :: tl -> next_token (Token_lbracket :: tokens) tl
    | ')' :: tl -> next_token (Token_rbracket :: tokens) tl
    | '+' :: tl -> next_token (Token_plus :: tokens) tl
    | '-' :: tl -> next_token (Token_minus :: tokens) tl
    | '*' :: tl -> next_token (Token_multiply :: tokens) tl
    | '/' :: tl -> next_token (Token_divide :: tokens) tl
    | c :: tl when Char.is_whitespace c -> next_token tokens tl
    | c :: tl when Char.is_digit c ->
        let num_list, tl = List.split_while ~f:(fun c -> Char.is_digit c) tl in
        let number = c :: num_list |> String.of_char_list |> Int.of_string in
        next_token (Token_number number :: tokens) tl
    | _ -> raise (Failure "unexpected character")
  in
  let reversed = next_token [] (String.to_list str) in
  List.rev reversed

let token_to_str = function
  | Token_number value -> Printf.sprintf "Token {type: number, val %d}" value
  | Token_lbracket -> "Token {type: lbracket, val: (}"
  | Token_rbracket -> "Token {type: rbracket, val: )}"
  | Token_plus -> "Token {type: plus, val: +}"
  | Token_minus -> "Token {type: minus, val: -}"
  | Token_multiply -> "Token {type: multiply, val: *}"
  | Token_divide -> "Token {type: divide, val: /}"

let print_token token = printf "%s\n" (token_to_str token)

let rec print_tokens_rec tokens =
  match tokens with
  | [] -> ()
  | token :: rest ->
      print_token token;
      print_tokens_rec rest

let print_tokens tokens = print_tokens_rec tokens
