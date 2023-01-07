(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Operator tree pretty printer goes here *)

open Base
open Meta
open Utils
open Interpret

let pp_node =
  let rec pp fmt { op; header } =
    let pp_items fmt projection =
      List.iteri
        ~f:(fun i (item, cname) ->
          let format =
            format_of_string
              (if i + 1 = List.length projection then "%s (%s)" else "%s (%s),@ ")
          in
          Format.fprintf fmt format cname (show_expression_type item))
        (Caml.List.combine projection header)
    in
    let pp_join_type fmt = function
      | Left _ -> Format.fprintf fmt "LEFT"
      | Right _ -> Format.fprintf fmt "RIGHT"
      | Inner _ -> Format.fprintf fmt "INNER"
      | Cross -> Format.fprintf fmt "CROSS"
    in
    let pp_join_constraint fmt = function
      | Left pred | Right pred | Inner pred ->
        Format.fprintf fmt "ON %s" (show_expression pred)
      | Cross -> ()
    in
    match op with
    | Projection { child; projection } ->
      Format.fprintf fmt "@[<v 4>Project @[%a@] ->@ %a@]@." pp_items projection pp child
    | Datasource { table } ->
      Format.fprintf fmt "@[<v 4>Datasource (%s)@]" (Table.name table)
    | Filter { child; filter } ->
      Format.fprintf fmt "@[<v 4>Filter (%s) ->@ %a@]" (show_expression filter) pp child
    | Join { left; right; join_constraint } ->
      Format.fprintf
        fmt
        "@[<v 4>%a JOIN@,%a@]@,@[<v 4>AND@,%a@]%a"
        pp_join_type
        join_constraint
        pp
        left
        pp
        right
        pp_join_constraint
        join_constraint
    | OrderBy { child; order_expr } -> failwith "orderby not implemented"
  in
  pp
;;

let pp = pp_node Format.std_formatter