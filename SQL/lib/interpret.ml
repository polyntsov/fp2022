(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Monadic interpreter of mini SQL *)

open Base
open Utils
open Meta

(* Column is represented by its index in the tuple, type parameter encodes the
   column type: int <-> IntCol ; string <-> StringCol
  *)
type 'a column = int

type 'a expression =
  | Column : 'a column -> 'a expression
  | ConstInt : int -> int expression
  | ConstString : string -> string expression
  | Plus : int expression * int expression -> int expression
  | Minus : int expression * int expression -> int expression
  | Mult : int expression * int expression -> int expression
  | Div : int expression * int expression -> int expression
  | Equal : 'a * 'a -> bool expression
  | NotEqual : 'a * 'a -> bool expression
  | Less : 'a * 'a -> bool expression
  | Greater : 'a * 'a -> bool expression
  | LessOrEq : 'a * 'a -> bool expression
  | GreaterOrEq : 'a * 'a -> bool expression

(* Since GADTs are not supported by variants lib *)
let plus l r = Plus (l, r)
let minus l r = Minus (l, r)
let mult l r = Mult (l, r)
let div l r = Div (l, r)
let eq l r = Equal (l, r)
let not_eq l r = NotEqual (l, r)
let less l r = Less (l, r)
let greater l r = Greater (l, r)
let less_or_eq l r = LessOrEq (l, r)
let greater_or_eq l r = GreaterOrEq (l, r)

type 'a projection =
  | Star
  | ProjExpr of 'a expression

(* Header of the relation is represented as a list of column names of
   the kind `tablename.columname`. This approach will not work for the aggregation or
   subqueries but since we don't support either such a representation should be enough.
   *)
type header = Ast.name list

let get_table_header table = Table.cols_as_fullnames table

type operator =
  | Projection of
      { child : node
      ; projection : Ast.projection_item list
      }
  | Join of
      { left : node
      ; right : node
      ; join_constraint : Ast.join_constraint
      }
  | Datasource of { table : table }
  | Filter of { child : node }
  | OrderBy of
      { child : node
      ; order_expr : Ast.orderby_clause list
      }

and node =
  { op : operator
  ; header : header
  }

let ( let* ) = Result.( >>= )

module QueryGenerator (M : MonadFail) (E : Environment) : sig
  val generate : Ast.statement -> (node, Utils.error) M.t
end = struct
  open M

  let resolve_table tname =
    try return (Meta.Database.get_table_ci tname E.db) with
    | Not_found -> fail (UnknownTable tname)
    | AmbiguousEntity -> fail (AmbiguousTable tname)
  ;;

  let rec transform_arithm_expression
    : type a. header -> Ast.arithm_expression -> (a expression, error) t
    =
   (* Why a cannot just specify it as a parameter? *)
   fun hdr ->
    let bin op l r =
      let* lexpr = transform_arithm_expression hdr l in
      let* rexpr = transform_arithm_expression hdr r in
      return (op lexpr rexpr)
    in
    function
    | Ast.Column fullname ->
      let* col =
        let resolve_col colname =
          try return (Database.get_col_ci colname E.db) with
          | Not_found -> fail (UnknownColumn fullname)
          | AmbiguousEntity -> fail (AmbiguousColumn fullname)
        in
        let resolve_col_with_table tname cname =
          let* table = resolve_table tname in
          try return (Table.get_col_ci cname table) with
          | Not_found -> fail (UnknownColumn fullname)
          | AmbiguousEntity -> fail (AmbiguousColumn fullname)
        in
        match Caml.String.split_on_char '.' fullname with
        | [ dbname; tname; cname ] ->
          if Database.get_name E.db != dbname
          then fail (WrongDatabase dbname)
          else resolve_col_with_table tname cname
        | [ tname; cname ] -> resolve_col_with_table tname cname
        | [ cname ] -> resolve_col cname
        | _ -> fail (UnknownColumn fullname)
      in
      (match column_type col with
       | IntCol -> return (Column (0 : int column))
       | StringCol -> return (Column (0 : string column)))
    | Ast.Int num -> return (ConstInt num)
    | Ast.Plus (l, r) -> bin plus l r
    | Ast.Minus (l, r) -> bin minus l r
    | Ast.Mult (l, r) -> bin mult l r
    | Ast.Div (l, r) -> bin div l r
 ;;

  let join_headers { header = lhdr } { header = rhdr } = lhdr @ rhdr
  let get_node_header { header } = header

  let cons_datasource table =
    { op = Datasource { table }; header = get_table_header table }
  ;;

  let cons_cross_join left right =
    { op = Join { left; right; join_constraint = Cross }
    ; header = join_headers left right
    }
  ;;

  let get_from_tables from =
    let rec helper = function
      | Ast.Table name ->
        let* table = resolve_table name in
        return [ table ]
      | Join { left; right } ->
        let* left_t = helper left in
        let* right_t = helper right in
        return (left_t @ right_t)
    in
    List.fold from ~init:(Ok []) ~f:(fun acc tables ->
      let* acc_t = acc in
      let* cur_t = helper tables in
      return (acc_t @ cur_t))
  ;;

  let generate = function
    | Ast.Insert -> assert false
    (* Insert queries are not supported yet and won't pass the parsing *)
    | Ast.Select { projection; from; where; orderby } ->
      let* from_tables = get_from_tables from in
      let* datasources = return (List.map from_tables ~f:cons_datasource) in
      let datasources_hd = List.hd_exn datasources in
      let datasources_tl = List.tl datasources in
      (match datasources_tl with
       | None -> return datasources_hd
       | Some tl -> return (List.fold tl ~f:cons_cross_join ~init:datasources_hd))
  ;;
end

module Interpret (M : MonadFail) (E : Environment) : sig
  val run : Ast.statement -> (Relation.t, Utils.error) M.t
end = struct
  open M

  let eval op = fail (UnknownColumn "not implemented")

  let run ast =
    let* plan =
      let module Generator = QueryGenerator (M) (E) in
      Generator.generate ast
    in
    eval plan
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
