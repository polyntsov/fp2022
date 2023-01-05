(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Meta

module Tuple : sig
  type t

  type element =
    | Int of int
    | String of string

  val from_string_list : string list -> table -> t
  val to_string_list : t -> string list
end

type t

val to_tuple_list : t -> Tuple.t list
val to_csv : t -> Csv.t

module AccessManager : sig
  type storage

  val load_db : database -> catalog -> storage
  val get_active_db : storage -> database
  val unset_active : storage option -> catalog -> unit
  val set_active : database -> storage option -> catalog -> storage
  val get_rel : table -> storage -> t
  val make_db_from : string -> catalog -> catalog
end