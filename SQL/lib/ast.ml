(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*

SQL-99 syntax:
SELECT [ DISTINCT | ALL ]
{Column expression [ AS name ]} [ ,... ] | *
FROM <Table reference> [ {,<Table reference>} ... ]
[ WHERE search condition ]
[ GROUP BY Columns [ HAVING condition ] ]
[ORDER BY {col_name | expr | position} [ASC | DESC],...]
[LIMIT {[offset,] row_count | row_count OFFSET offset}]
[PROCEDURE procedure_name(argument_list)]
[INTO OUTFILE 'file_name' export_options |
 INTO DUMPFILE 'file_name' |
 INTO var_name [, var_name]]
[FOR UPDATE | LOCK IN SHARE MODE]

We support only a subset of it:
SELECT
{Column expression [ AS name ]} [ ,... ] | *
FROM <Table reference> [ {,<Table reference>} ... ]
[ WHERE search condition ]
[ORDER BY {col_name | expr} [ASC | DESC],...]

Table reference can be either a table name (without alias)
represented as a string of english letters or a join clause.
Join clause:
  <Table reference> [JOIN TYPE] JOIN <Table reference> ON <join condition>
OR:
  <Table reference> CROSS JOIN <Table reference>

*)

type name = string [@@deriving show { with_path = false }]

type arithm_expression =
  | Column of name
  | Int of int
  | Plus of arithm_expression * arithm_expression
  | Minus of arithm_expression * arithm_expression
  | Mult of arithm_expression * arithm_expression
  | Div of arithm_expression * arithm_expression
[@@deriving show { with_path = false }, variants]

type expression =
  | Arithm of arithm_expression
  (* To forbid ill-formed arithmetic expressions with strings on the parser level *)
  | String of string (** Any single quoted string *)
[@@deriving show { with_path = false }, variants]

type projection_item =
  | Star
  | AtomItem of expression * name option
[@@deriving show { with_path = false }, variants]

type predicate =
  | OrPred of predicate * predicate
  | AndPred of predicate * predicate
  | Equal of expression * expression
  | NotEqual of expression * expression
  | Less of expression * expression
  | Greater of expression * expression
  | LessOrEq of expression * expression
  | GreaterOrEq of expression * expression
[@@deriving show { with_path = false }, variants]

type where_clause = predicate [@@deriving show { with_path = false }]
type orderby_item = expression [@@deriving show { with_path = false }]

type orderby_clause =
  | Asc of orderby_item
  | Desc of orderby_item
[@@deriving show { with_path = false }]

type join_constraint =
  | Left of predicate
  | Right of predicate
  | Inner of predicate
  | Cross
[@@deriving show { with_path = false }]

type datasource =
  | Table of name
  | Join of
      { left : datasource
      ; right : datasource
      ; join_type : join_constraint
      }
[@@deriving show { with_path = false }]

type from_clause =
  { from : datasource list
  ; where : where_clause option
  ; orderby : orderby_clause option
  }
[@@deriving show { with_path = false }]

type statement =
  | Insert
  | Select of projection_item list * from_clause
[@@deriving show { with_path = false }]
