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
   select * from t1;
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

module type Environment = sig
  val catalog_path : string
  val catalog : catalog
  val storage : Relation.AccessManager.storage
end

module Datasource (M : MonadFail) (E : Environment) : SDatasource = struct
  open M

  let cons_execute tablename () = fail (UnknownTable tablename)
  (*let table =
      match Catalog.get_table tablename (Catalog.load ".") with
      | Some t -> return t
      | None -> fail (UnknownTable tablename)
    in
    table >>| Relation.load*)
end

module QueryGenerator (M : MonadFail) (E : Environment) : sig
  val generate : Ast.statement -> (plan, Utils.error) M.t
end = struct
  let get_from_tables from =
    let open Result in
    let rec helper = function
      | Table name ->
        (match Catalog.get_table_ci name E.catalog with
         | [] -> Error (UnknownTable name)
         | _t1 :: _t2 :: _ -> Error (AmbiguousTable name)
         | t -> Ok t)
      | Join { left; right } ->
        helper left >>= fun head -> helper right >>= fun tail -> Ok (head @ tail)
    in
    List.fold from ~init:(Ok []) ~f:(fun acc tables ->
      acc >>= fun acc -> helper tables >>= fun tables -> Ok (acc @ tables))
  ;;

  let generate ast =
    let module DS = Datasource (M) (E) in
    M.return (DS.cons_execute "t1")
  ;;
end

module Interpret (M : MonadFail) (E : Environment) : sig
  val run : Ast.statement -> (Relation.t, Utils.error) M.t
end = struct
  open M

  let run ast =
    let plan =
      let module Generator = QueryGenerator (M) (E) in
      Generator.generate ast
    in
    plan >>= fun plan -> plan ()
  ;;
end

let interpret query (module E : Environment) =
  let ans =
    let module I = Interpret (Result) (E) in
    match Parser.parse query with
    | Caml.Result.Ok ast -> I.run ast
    | Caml.Result.Error error -> Result.fail (ParsingError error)
  in
  ans
;;
