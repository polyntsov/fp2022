(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(*** Test helper functions ***)

let parse_string parser s = Angstrom.parse_string ~consume:All parser s

let assert_ok show parser s expected =
  match parse_string parser s with
  | Result.Ok actual when actual = expected -> true
  | Result.Ok actual ->
    Format.printf
      "When parsing '%s':\nExpected\n\t%s\ngot\n\t%s\n"
      s
      (show expected)
      (show actual);
    false
  | Result.Error msg ->
    Format.printf "Parsing of '%s' failed with error %s\n" s msg;
    false
;;

let assert_ok_s parser s = assert_ok Fun.id parser s s

let assert_error parser s =
  match parse_string parser s with
  | Result.Error _ -> true
  | Result.Ok _ -> false
;;

(*** Small auxiliary parsers ***)

(* Returns true if provided char is any kind of whitespace *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(* Skips spaces *)
let spaces_p = skip_while is_space
let lspaces p = spaces_p *> p
let rspaces p = p <* spaces_p
let spaces p = spaces_p *> p <* spaces_p

(* Parses dot (symbol '.') *)
let dot_p =
  peek_char
  >>= function
  | Some '.' -> advance 1 >>| fun () -> true
  | _ -> return false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* Parses any string containing only letters and digits.
   The string must start with a letter. *)
let entity_name_p =
  let is_letter = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
  in
  (peek_char
  >>= function
  | Some c when is_letter c -> return (String.make 1 c)
  | Some c -> fail ("Entity name cannot start with " ^ String.make 1 c)
  | _ -> fail "Entity name cannot be empty")
  *> take_while1 (fun c -> is_letter c || is_digit c)
;;

(* Takes a parser and returns a new parser that is able to parse [topname.]name where
   [topname.] is parsed using top_p *)
let complex_entity_name_p top_p =
  top_p
  >>= fun name ->
  dot_p
  >>= function
  | false -> return name
  | true -> entity_name_p >>| Format.sprintf "%s.%s" name
;;

(*** Expression parsers ***)

(* Table name parser, parses [database.]table strings *)
let table_name_p = complex_entity_name_p entity_name_p

let%test _ = assert_ok_s table_name_p "main"
let%test _ = assert_ok_s table_name_p "MaIn"
let%test _ = assert_ok_s table_name_p "maindb.main"
let%test _ = assert_error table_name_p ".main"
let%test _ = assert_error table_name_p "maindb..main"
let%test _ = assert_error table_name_p "Main."
let%test _ = assert_error table_name_p "maindb.main."
let%test _ = assert_error table_name_p "1table"

(* Column name parser, parses [[database.]table.]column strings *)
let column_name_p = complex_entity_name_p table_name_p

let%test _ = assert_ok_s column_name_p "main"
let%test _ = assert_ok_s column_name_p "main.a"
let%test _ = assert_ok_s column_name_p "main.main.a"
let%test _ = assert_error column_name_p "main.main.a."
let%test _ = assert_error column_name_p ".main.main.a"

let table_p =
  let table_p = table_name_p >>| fun name -> Table name in
  lspaces table_p
;;

let column_p =
  let column_p = column_name_p >>| column in
  lspaces column_p
;;

(* Parser for integer sign *)
let sign_p =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> "-"
  | Some '+' -> advance 1 >>| fun () -> "+"
  | Some c when is_digit c -> return "+"
  | _ -> fail "Sign or digit expected"
;;

(* Parser of integer value *)
let integer_p =
  let integer_p =
    sign_p
    >>= fun sign ->
    take_while1 is_digit >>| fun str_int -> Int (int_of_string (sign ^ str_int))
  in
  lspaces integer_p
;;

let assert_ok_int s expected = assert_ok show_arithm_expression integer_p s expected

let%test _ = assert_ok_int "+10" (Int 10)
let%test _ = assert_ok_int " +10" (Int 10)
let%test _ = assert_ok_int "-10" (Int (-10))
let%test _ = assert_ok_int "42" (Int 42)
let%test _ = assert_error integer_p "42-"
let%test _ = assert_error integer_p "--42"
let%test _ = assert_error integer_p "42."

let single_quote = '\''
let single_quote_p = char single_quote

(* Parses single quoted string *)
let string_value_p =
  let string_value_p =
    single_quote_p *> take_while (( != ) single_quote) <* single_quote_p >>| string
  in
  lspaces string_value_p
;;

let parens_p p = lspaces (char '(' *> p <* char ')')

let infix_op_p str_op cons =
  let infix_op_p = Angstrom.string str_op *> return cons in
  lspaces infix_op_p
;;

let add_p = infix_op_p "+" plus
let sub_p = infix_op_p "-" minus
let mul_p = infix_op_p "*" mult
let div_p = infix_op_p "/" div

let chainl1_p e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let arithm_expr_p =
  fix (fun expr ->
    let factor = parens_p expr <|> integer_p <|> column_p in
    let term = chainl1_p factor (mul_p <|> div_p) in
    chainl1_p term (add_p <|> sub_p))
;;

let expr_p = arithm_expr_p >>| arithm <|> string_value_p
let assert_ok_expr s expected = assert_ok show_expression expr_p s expected

let%test _ = assert_ok_expr "'abc'" (String "abc")
let%test _ = assert_ok_expr "'  kakadu'" (String "  kakadu")
let%test _ = assert_ok_expr "    'k a k a d u'" (String "k a k a d u")
let%test _ = assert_ok_expr "'r_d_b_m_s'" (String "r_d_b_m_s")
let%test _ = assert_ok_expr "'\"kakadu\"'" (String "\"kakadu\"")
let%test _ = assert_ok_expr "''" (String "")
let%test _ = assert_ok_expr "1 + 1" (Arithm (Plus (Int 1, Int 1)))
let%test _ = assert_ok_expr "1 * 1" (Arithm (Mult (Int 1, Int 1)))
let%test _ = assert_ok_expr "1 *  8" (Arithm (Mult (Int 1, Int 8)))
let%test _ = assert_ok_expr "1 - column" (Arithm (Minus (Int 1, Column "column")))
let%test _ = assert_ok_expr "A / B" (Arithm (Div (Column "A", Column "B")))
let%test _ = assert_ok_expr "  A/43" (Arithm (Div (Column "A", Int 43)))
(* It works like this in postgres, so I'll leave it as it is *)
let%test _ = assert_ok_expr "1+-1" (Arithm (Plus (Int 1, Int (-1))))

let%test _ =
  assert_ok_expr
    "1*8 + 3 + (2 * 4)"
    (Arithm (Plus (Plus (Mult (Int 1, Int 8), Int 3), Mult (Int 2, Int 4))))
;;

let%test _ =
  assert_ok_expr
    "1*8 + 3 + (tablename.column * 4)"
    (Arithm
       (Plus (Plus (Mult (Int 1, Int 8), Int 3), Mult (Column "tablename.column", Int 4))))
;;

let%test _ =
  assert_ok_expr
    "1 * (8 + 3) - db.table.col"
    (Arithm (Minus (Mult (Int 1, Plus (Int 8, Int 3)), Column "db.table.col")))
;;

let%test _ = assert_error expr_p "1 + 'abc'"
let%test _ = assert_error expr_p "'test'test'"
let%test _ = assert_error expr_p "'''"
let%test _ = assert_error expr_p "1 * (8 + 3) - db.db.table.col"

(*** Projection item parsers ***)
let star_p = lspaces (char '*')
let as_p = spaces (string_ci "AS")

let alias_p =
  option "" as_p
  >>= function
  | "" -> return None
  | _ -> entity_name_p >>| fun name -> Some name
;;

let single_item_p = lift2 atomitem expr_p alias_p
let proj_item_p = star_p *> return Star <|> single_item_p
let assert_ok_proj s expected = assert_ok show_projection_item proj_item_p s expected

let%test _ = assert_ok_proj "*" Star
let%test _ = assert_ok_proj " *" Star
let%test _ = assert_ok_proj "1 +3" (AtomItem (Arithm (Plus (Int 1, Int 3)), None))
let%test _ = assert_ok_proj "col as A" (AtomItem (Arithm (Column "col"), Some "A"))

let%test _ =
  assert_ok_proj
    "2 * col as Alias"
    (AtomItem (Arithm (Mult (Int 2, Column "col")), Some "Alias"))
;;

(*** Predicate parsers ***)
let equal_p = infix_op_p "=" equal
let not_equal_p = infix_op_p "!=" notequal
let less_p = infix_op_p "<" less
let greater_p = infix_op_p ">" greater
let less_or_eq_p = infix_op_p "<=" lessoreq
let greater_or_eq_p = infix_op_p ">=" greateroreq

let atom_predicate_p =
  let pred_p =
    equal_p <|> not_equal_p <|> less_or_eq_p <|> greater_or_eq_p <|> less_p <|> greater_p
  in
  lift3 (fun x pred y -> pred x y) expr_p pred_p expr_p
;;

let and_p = infix_op_p "AND" andpred
let or_p = infix_op_p "OR" orpred

let predicate_p =
  fix (fun expr ->
    let factor = parens_p expr <|> atom_predicate_p in
    let term = chainl1_p factor and_p in
    chainl1_p term or_p)
;;

let assert_ok_pred s expected = assert_ok show_predicate predicate_p s expected

let%test _ = assert_ok_pred "1 < 2" (Less (Arithm (Int 1), Arithm (Int 2)))
let%test _ = assert_ok_pred "1 > A" (Greater (Arithm (Int 1), Arithm (Column "A")))
let%test _ = assert_ok_pred "1=23" (Equal (Arithm (Int 1), Arithm (Int 23)))
let%test _ = assert_ok_pred "A<=B" (LessOrEq (Arithm (Column "A"), Arithm (Column "B")))
let%test _ = assert_ok_pred "A >= 0" (GreaterOrEq (Arithm (Column "A"), Arithm (Int 0)))
let%test _ = assert_ok_pred " i  != 0" (NotEqual (Arithm (Column "i"), Arithm (Int 0)))

let%test _ =
  assert_ok_pred
    "t1.id = t2.id"
    (Equal (Arithm (Column "t1.id"), Arithm (Column "t2.id")))
;;

let%test _ = assert_ok_pred "'abc' = col" (Equal (String "abc", Arithm (Column "col")))

let%test _ =
  assert_ok_pred
    "1 + 1 >= 2 AND A < B"
    (AndPred
       ( GreaterOrEq (Arithm (Plus (Int 1, Int 1)), Arithm (Int 2))
       , Less (Arithm (Column "A"), Arithm (Column "B")) ))
;;

let%test _ =
  assert_ok_pred
    "2 * B < C OR 1 + 1 >= 2 AND A < B"
    (OrPred
       ( Less (Arithm (Mult (Int 2, Column "B")), Arithm (Column "C"))
       , AndPred
           ( GreaterOrEq (Arithm (Plus (Int 1, Int 1)), Arithm (Int 2))
           , Less (Arithm (Column "A"), Arithm (Column "B")) ) ))
;;

let%test _ =
  assert_ok_pred
    "(2 * B < C OR 1 + 1 >= 2) AND A < B"
    (AndPred
       ( OrPred
           ( Less (Arithm (Mult (Int 2, Column "B")), Arithm (Column "C"))
           , GreaterOrEq (Arithm (Plus (Int 1, Int 1)), Arithm (Int 2)) )
       , Less (Arithm (Column "A"), Arithm (Column "B")) ))
;;

type error = [ `ParsingError of string ]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

(*let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ char '(' *> pack.apps pack <* char ')'
        ; ((string "λ" <|> string "\\") *> spaces *> varname
          <* spaces
          <* char '.'
          >>= fun var ->
          pack.apps pack >>= fun b -> return (Ast.Abs (String.make 1 var, b)))
        ; (varname <* spaces >>= fun c -> return (Ast.Var (String.make 1 c)))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  in
  { single; apps }
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`ParsingError er)
;;

let parse_optimistically str = Result.get_ok (parse str)
let pp = Printast.pp_named

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "x y");
  [%expect {| (App ((Var x), (Var y))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(x y)");
  [%expect {| (App ((Var x), (Var y))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(\\x . x x)");
  [%expect {| (Abs (x, (App ((Var x), (Var x))))) |}]
;;

let%expect_test _ =
  Format.printf "%a" pp (parse_optimistically "(λf.λx. f (x x))");
  [%expect {| (Abs (f, (Abs (x, (App ((Var f), (App ((Var x), (Var x))))))))) |}]
;;*)
