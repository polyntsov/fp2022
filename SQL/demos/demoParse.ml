(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Sql_lib

let () =
  let s = Stdio.In_channel.input_all Caml.stdin in
  match Sql_lib.Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n%!" Printast.pp_named ast
  | Error _ -> Format.printf "Some error"
;;
