open Token
open Base

type ast = Number of int | Operation of ast * token * ast

let rec ast_to_str = function
  | Operation (left, op, right) ->
      ast_to_str left ^ "\n" ^ token_to_str op ^ "\n" ^ ast_to_str right
  | Number x -> Int.to_string x
