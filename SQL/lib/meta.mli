(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Functions and types to work with DBMS entities such as catalog, databases, tables and columns *)

(** Type of values that the column stores *)
type column_type =
  | IntCol
  | StringCol
[@@deriving yojson]

(** Table attirbute, constitutes column's name and type *)
type column

(** Header of the table, i.e. list of attributes *)
type header

(** Table in the database represented by a name and a header *)
type table

(** Database represented as a named list of tables *)
type database

(** Catalog represented as list of databases, there should be only one catalog *)
type catalog
(** Catalog is stored with this directory structure:
   _catalog/┌──►Catalog.meta.json
            │
            ├──►Database1_name/┌───►Table1.csv
            │                  │
            │                  └───►Table2.csv
            │
            └──►Database2_name/┌───►Table1.csv
                               │
                               ├───►Table2.csv
                               │
                               └───►Table3.csv
  Catalog.meta.json contains all information about this catalog,
  i.e. all databases (identified by their names), their tables, headers of the tables
*)

type tuple_element =
  | Int of int
  | String of string

module Tuple : sig
  type t

  val from_string : string list -> header -> t
  val to_string : t -> string list
end

module Relation : sig
  type t

  val load : table -> t
  val to_tuple_list : t -> Tuple.t list
end

module Catalog : sig
  (** [create ()] loads existing catalog or creates a new empty one if none exists *)
  val create : unit -> catalog

  (** [drop ()] deletes current catalog and returns a new empty one *)
  val drop : unit -> catalog

  (** [create_db name c] creates a new empty database named [name] in corresponding catalog [c] *)
  val create_db : string -> catalog -> database * catalog

  (** [create_table name db] creates a new empty table named [name] in corresponding database [db] *)
  val create_table : string -> database -> table * catalog

  (** [create_cols cols_list table] creates columns from [cols_list] and inserts them into [table]  *)
  val create_cols : (string * column_type) list -> table -> table * catalog

  (** [dump c] converts [c] to json and writes it to the disk *)
  val dump : catalog -> unit

  (** [to_string c] converts [c] to json string *)
  val to_string : catalog -> string

  (** [get_table name c] returns [Some table] with name [name] or [None] if none exists *)
  val get_table : string -> catalog -> table option
end
