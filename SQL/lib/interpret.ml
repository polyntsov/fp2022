(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Real monadic interpreter goes here *)

open Base
open Utils
open Meta
open Ast

type projection_item
type join_type
type table
type order_expr

(* преобразование от аст к интепретируемому виду?
   там проверки типов + поиск нужных таблиц и колонок по каталогу.
   Как должны выглядеть исполняемые предикаты? Колонки или индексы?
*)

type expression =
  | Const of int
  | Column of column
  | Plus of expression * expression
  | Minus of expression * expression
  | Mult of expression * expression
  | Div of expression * expression

type predicate =
  | OrPred of predicate * predicate
  | AndPred of predicate * predicate
  | Equal of expression * expression
  | NotEqual of expression * expression
  | Less of expression * expression
  | Greater of expression * expression
  | LessOrEq of expression * expression
  | GreaterOrEq of expression * expression

type operator =
  | Projection of
      { child : operator
      ; projection : projection_item list
      }
  | Join of
      { left : operator
      ; right : operator
      ; join_type : join_type
      ; pred : predicate
      }
  | OpDatasource of { table : table }
  | Filter of
      { child : operator
      ; pred : predicate
      }
  | OrderBy of
      { child : operator
      ; order_expr : order_expr
      }

(*let transform ast c =
  let rec ds_collect_tables = function
    | Table name ->
      (match Catalog.get_table name c with
       | Some t -> t
       | None -> M.fail (UnknownVariable ""))
       | Join { left; right; _ } -> ds_collect_tables left @ ds_collect_tables right)
  in
  ()
;;*)

module GenerateFail = struct end

type plan = unit -> (Relation.t, error) result

module type Operator = sig
  type arg

  val cons_execute : arg -> plan
end

module type UnaryOperator = sig
  include Operator

  type child

  val cons_execute : child -> arg -> plan
end

module type BinaryOperator = sig
  include Operator

  type left
  type right

  val cons_execute : left -> right -> arg -> plan
end

module type SDatasource = Operator with type arg := name

module Datasource (M : MonadFail) (C : SCatalog) : SDatasource = struct
  open M

  let cons_execute tablename () =
    let table =
      match C.get_table tablename (C.create ()) with
      | Some t -> return t
      | None -> fail (UnknownTable tablename)
    in
    table >>| Relation.load
  ;;
end

module QueryGenerator (M : MonadFail) (C : SCatalog) : sig
  val generate : Ast.statement -> (plan, Utils.error) M.t
end = struct
  let generate ast =
    let module DS = Datasource (M) (C) in
    M.return (DS.cons_execute "t1")
  ;;
end

module Interpret (M : MonadFail) (C : SCatalog) : sig
  val run : Ast.statement -> (Relation.t, Utils.error) M.t
end = struct
  open M

  let run ast =
    let plan =
      let module Generator = QueryGenerator (M) (C) in
      Generator.generate ast
    in
    plan >>= fun plan -> plan ()
  ;;
end

(*let parse_and_run str =
  let ans =
    let module I = Interpret (Result) (Catalog) in
    match Parser.parse str with
    | Caml.Result.Ok ast ->
      (match I.run ast with
       | Caml.Result.Ok rel -> rel
       | Caml.Result.Error msg ->
         Caml.Format.eprintf "Interpretation error: %s\n%!" msg;
         Caml.exit 1)
    | Caml.Result.Error msg ->
      Caml.Format.eprintf "Parsing error: %s\n%!" msg;
      Caml.exit 1
  in
  ans
;;*)
