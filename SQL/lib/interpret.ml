(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Monadic interpreter of mini SQL *)

open Base
open Utils
open Meta

(* Column is represented by its index in the tuple *)
type column = int

type 'a expression =
  | IntCol : column -> int expression
  | StringCol : column -> string expression
  | ConstInt : int -> int expression
  | ConstString : string -> string expression
  | Plus : int expression * int expression -> int expression
  | Minus : int expression * int expression -> int expression
  | Mult : int expression * int expression -> int expression
  | Div : int expression * int expression -> int expression
  | Equal : 'a expression * 'a expression -> bool expression
  | NotEqual : 'a expression * 'a expression -> bool expression
  | Less : 'a expression * 'a expression -> bool expression
  | Greater : 'a expression * 'a expression -> bool expression
  | LessOrEq : 'a expression * 'a expression -> bool expression
  | GreaterOrEq : 'a expression * 'a expression -> bool expression
  | Or : bool expression * bool expression -> bool expression
  | And : bool expression * bool expression -> bool expression

let rec show_expression : type a. a expression -> _ =
  let bin cons_s l r =
    Format.sprintf "%s (%s, %s)" cons_s (show_expression l) (show_expression r)
  in
  function
  | IntCol column -> Format.sprintf "IntCol %d" column
  | StringCol column -> Format.sprintf "StringCol %s" (string_of_int column)
  | ConstInt i -> Format.sprintf "ConstInt %d" i
  | ConstString s -> Format.sprintf "ConstString %s" s
  | Plus (l, r) -> bin "Cons" l r
  | Minus (l, r) -> bin "Minus" l r
  | Mult (l, r) -> bin "Mult" l r
  | Div (l, r) -> bin "Div" l r
  | Equal (l, r) -> bin "Equal" l r
  | NotEqual (l, r) -> bin "NotEqual" l r
  | Less (l, r) -> bin "Less" l r
  | Greater (l, r) -> bin "Greater" l r
  | LessOrEq (l, r) -> bin "LessOrEq" l r
  | GreaterOrEq (l, r) -> bin "GreaterOrEq" l r
  | Or (l, r) -> bin "Or" l r
  | And (l, r) -> bin "And" l r
;;

type expression_type =
  [ `Int of int expression
  | `String of string expression
  | `Bool of bool expression
  ]

let show_expression_type = function
  | `Int (i : int expression) -> show_expression i
  | `String (s : string expression) -> show_expression s
  | `Bool (b : bool expression) -> show_expression b
;;

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
let or_pred l r = Or (l, r)
let and_pred l r = And (l, r)

type pred_cons_helper =
  { pred_cons : 'a. 'a expression -> 'a expression -> bool expression }

type 'a projection =
  | Star
  | ProjExpr of 'a expression

(* Header of the relation (and of the operator) is represented as a list of column names of
   the kind `tablename.columname`. This approach will not work for the aggregation or
   subqueries but since we don't support either such a representation should be enough.
   Also it's not the most efficient one (because of the linear search on column name, but I
   don't think it's an important issue now)
   *)
type header = Ast.name list

let get_table_header table = Table.cols_as_fullnames table

let header_findi value hdr =
  let rec helper lst curi =
    match lst with
    | [] -> raise Not_found
    | hd :: tl -> if String.( = ) hd value then curi else helper tl (curi + 1)
  in
  helper hdr 0
;;

type operator =
  | Projection of
      { child : node
      ; projection : expression_type list
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

let rec show_plan { op; header } =
  match op with
  | Projection { child; projection } ->
    let pp_items =
      List.map2_exn
        ~f:(fun item cname -> Format.sprintf "%s (%s)" cname (show_expression_type item))
        projection
        header
    in
    Format.sprintf
      "Projection: %s\n%s"
      (String.concat ~sep:", " pp_items)
      (show_plan child)
  | Datasource { table } -> Format.sprintf "Datasource (%s)" (Table.name table)
  | Filter { child } -> failwith "filter not implemented"
  | Join { left; right; join_constraint } ->
    Format.sprintf "CROSS JOIN\n%s\nAND\n%s\n" (show_plan left) (show_plan right)
  | OrderBy { child; order_expr } -> failwith "orderby not implemented"
;;

let ( let* ) = Result.( >>= )

module QueryGenerator (M : MonadFail) (E : Environment) : sig
  val generate : Ast.statement -> (node, Utils.error) M.t
end = struct
  open M

  type table_col =
    { table : Meta.table
    ; col : Meta.column
    }

  let header_findi_by_col { table; col } hdr =
    let cname = Table.col_fullname col table in
    header_findi cname hdr
  ;;

  let resolve_table tname =
    try return (Meta.Database.get_table_ci tname E.db) with
    | Not_found -> fail (UnknownTable tname)
    | AmbiguousEntity -> fail (AmbiguousTable tname)
  ;;

  let types_mismatch l r =
    TypesMismatch (Format.sprintf "'%s' and '%s' have incompatible types" l r)
  ;;

  let expr_types_mismatch l r =
    types_mismatch (show_expression_type l) (show_expression_type r)
  ;;

  let col_to_expression_type col i =
    match column_type col with
    | Meta.IntCol -> `Int (IntCol i)
    | Meta.StringCol -> `String (StringCol i)
  ;;

  let rec transform_arithm_expression hdr tables =
    let bin op l r =
      let* lexpr = transform_arithm_expression hdr tables l in
      let* rexpr = transform_arithm_expression hdr tables r in
      match lexpr, rexpr with
      | `Int lexpr, `Int rexpr -> return (`Int (op lexpr rexpr))
      | _ -> fail (expr_types_mismatch lexpr rexpr)
    in
    function
    | Ast.Column fullname ->
      let* table, col =
        let resolve_with f cname entity =
          try return (f cname entity) with
          | Not_found -> fail (UnknownColumn fullname)
          | AmbiguousEntity -> fail (AmbiguousColumn fullname)
        in
        let resolve_col_with_table tname cname =
          let* table = resolve_with Table.get_table_by_name tname tables in
          resolve_with
            (fun cname table -> table, Table.get_col_ci cname table)
            cname
            table
        in
        match Caml.String.split_on_char '.' fullname with
        | [ dbname; tname; cname ] ->
          if Database.get_name E.db != dbname
          then fail (WrongDatabase dbname)
          else resolve_col_with_table tname cname
        | [ tname; cname ] -> resolve_col_with_table tname cname
        | [ cname ] -> resolve_with Database.get_col_ci cname tables
        (* actually impossible case, because such names should not pass parsing *)
        | _ -> assert false
      in
      return (col_to_expression_type col (header_findi_by_col { table; col } hdr))
    | Ast.Int num -> return (`Int (ConstInt num))
    | Ast.Plus (l, r) -> bin plus l r
    | Ast.Minus (l, r) -> bin minus l r
    | Ast.Mult (l, r) -> bin mult l r
    | Ast.Div (l, r) -> bin div l r
  ;;

  let transform_atom_expression hdr tables = function
    | Ast.String e -> return (`String (ConstString e))
    | Ast.Arithm e -> transform_arithm_expression hdr tables e
  ;;

  let rec transform_predicate hdr tables =
    let simple_pred { pred_cons } l r =
      let* l_atom_expr = transform_atom_expression hdr tables l in
      let* r_atom_expr = transform_atom_expression hdr tables r in
      match l_atom_expr, r_atom_expr with
      | `Int l, `Int r -> return (pred_cons l r)
      | `String l, `String r -> return (pred_cons l r)
      | l, r -> fail (expr_types_mismatch l r)
    in
    let pred_pred { pred_cons } l r =
      let* l_pred = transform_predicate hdr tables l in
      let* r_pred = transform_predicate hdr tables r in
      return (pred_cons l_pred r_pred)
    in
    let complex_pred cons l r =
      let* l_pred = transform_predicate hdr tables l in
      let* r_pred = transform_predicate hdr tables r in
      return (cons l_pred r_pred)
    in
    function
    | Ast.Equal (l, r) -> simple_pred { pred_cons = eq } l r
    | Ast.NotEqual (l, r) -> simple_pred { pred_cons = not_eq } l r
    | Ast.Less (l, r) -> simple_pred { pred_cons = less } l r
    | Ast.Greater (l, r) -> simple_pred { pred_cons = greater } l r
    | Ast.LessOrEq (l, r) -> simple_pred { pred_cons = less_or_eq } l r
    | Ast.GreaterOrEq (l, r) -> simple_pred { pred_cons = greater_or_eq } l r
    | Ast.PredEqual (l, r) -> pred_pred { pred_cons = eq } l r
    | Ast.PredNotEqual (l, r) -> pred_pred { pred_cons = not_eq } l r
    | Ast.PredLess (l, r) -> pred_pred { pred_cons = less } l r
    | Ast.PredGreater (l, r) -> pred_pred { pred_cons = greater } l r
    | Ast.PredLessOrEq (l, r) -> pred_pred { pred_cons = less_or_eq } l r
    | Ast.PredGreaterOrEq (l, r) -> pred_pred { pred_cons = greater_or_eq } l r
    | Ast.OrPred (l, r) -> complex_pred or_pred l r
    | Ast.AndPred (l, r) -> complex_pred and_pred l r
  ;;

  let transform_projection_item hdr tables = function
    | Ast.Star ->
      return
        (List.mapi
           ~f:(fun i fullname ->
             let col = Database.get_col_by_fullname_ci fullname E.db in
             col_to_expression_type col i)
           hdr)
    | ProjAtomItem (expr, _alias) ->
      (match expr with
       | AtomExpr atom_expr ->
         let* expr = transform_atom_expression hdr tables atom_expr in
         return [ expr ]
       | PredExpr pred_expr ->
         let* expr = transform_predicate hdr tables pred_expr in
         return [ `Bool expr ])
  ;;

  let calc_projection_header hdr =
    (* This is the name postgres uses for expressions without alias *)
    let dummy_name = "?column?" in
    let get_cname = function
      | Ast.PredExpr _ -> dummy_name
      | Ast.AtomExpr expr ->
        (match expr with
         | Ast.String _ -> dummy_name
         | Ast.Arithm expr ->
           (match expr with
            | Column name -> name
            | _ -> dummy_name))
    in
    function
    | Ast.Star -> hdr
    | ProjAtomItem (expr, alias) ->
      (match alias with
       | Some name -> [ name ]
       | None -> [ get_cname expr ])
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

  let cons_projection ({ header = child_hdr } as child) proj_items tables =
    (* not very efficient, but concise *)
    let* projections =
      M.all (List.map ~f:(transform_projection_item child_hdr tables) proj_items)
    in
    let projection = List.concat projections in
    return
      { op = Projection { child; projection }
      ; header = List.concat (List.map ~f:(calc_projection_header child_hdr) proj_items)
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
    (* Insert queries are not supported yet and won't pass the parsing *)
    | Ast.Insert -> assert false
    | Ast.Select { projection; from; where; orderby } ->
      let* from_tables = get_from_tables from in
      let datasources = List.map from_tables ~f:cons_datasource in
      let datasources_hd = List.hd_exn datasources in
      let datasources_tl = List.tl datasources in
      let tree =
        match datasources_tl with
        | None -> datasources_hd
        | Some tl -> List.fold tl ~f:cons_cross_join ~init:datasources_hd
      in
      cons_projection tree projection from_tables
  ;;
end

module Interpret (M : MonadFail) (E : Environment) : sig
  val run : Ast.statement -> (Relation.t, Utils.error) M.t
  val explain : Ast.statement -> (string, Utils.error) M.t
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

  let explain ast =
    let* plan =
      let module Generator = QueryGenerator (M) (E) in
      Generator.generate ast
    in
    return (show_plan plan)
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

let explain query (module E : Environment) =
  let ans =
    let module I = Interpret (Result) (E) in
    match Parser.parse query with
    | Caml.Result.Ok ast -> I.explain ast
    | Caml.Result.Error error -> Result.fail (ParsingError error)
  in
  ans
;;
