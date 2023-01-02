(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type column_type =
  | IntCol
  | StringCol
[@@deriving yojson]

type column =
  { cname : string
  ; ctype : column_type
  ; table : table
  }
[@@deriving yojson]

and header = column list [@@deriving yojson]

and table =
  { tname : string
  ; db : database
  ; header : header
  }
[@@deriving yojson]

and database =
  { dname : string
  ; catalog : catalog
  ; tables : table list
  }
[@@deriving yojson]

and catalog = Catalog of database list [@@deriving yojson]

module Database = struct
  let get_name { dname } = dname
  let get_catalog { catalog } = catalog
  let table_exists table { tables } = List.mem table tables

  let load path =
    let load_table tpath = Csv.load tpath in
    load_table path
  ;;

  let add_table table { dname; catalog; tables } =
    { dname; catalog; tables = table :: tables }
  ;;

  let delete_table table { dname; catalog; tables } =
    { dname; catalog; tables = List.filter (( != ) table) tables }
  ;;

  let create_table tname db =
    let t = { tname; db; header = [] } in
    t, add_table t db
  ;;

  let dump path db = Sys.mkdir (Filename.concat path (get_name db)) 0o775
  let update_table table db = delete_table table db |> add_table table
  let find_table name { tables } = List.find_opt (fun { tname } -> name = tname) tables
end

module Table = struct
  let create_col cname coltype ({ header } as table) =
    let col = { cname; ctype = coltype; table } in
    col, { table with header = col :: header }
  ;;

  let rec create_cols args table =
    List.fold_right
      (fun (cname, coltype) table -> snd (create_col cname coltype table))
      args
      table
  ;;

  let get_fullname { tname; db } = Printf.sprintf "%s.%s" (Database.get_name db) tname
  let get_name { tname } = tname
  let get_header { header } = header
  let get_db { db } = db

  let get_column_names table =
    get_header table |> fun cols -> List.map (fun { cname } -> cname) cols
  ;;

  let add_column col { tname; db; header = cols } = { tname; db; header = col :: cols }
  let rec add_columns cols table = List.fold_right add_column cols table
end

let rec rm_non_empty_dir path =
  match Sys.is_directory path with
  | true ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_non_empty_dir (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path
;;

(* Copy-paste from .mli file to make it compile (why does it work like this?) *)
module type SCatalog = sig
  val create : unit -> catalog
  val drop : unit -> catalog
  val create_db : string -> catalog -> database * catalog
  val create_table : string -> database -> table * catalog
  val create_cols : (string * column_type) list -> table -> table * catalog
  val dump : catalog -> unit
  val to_string : catalog -> string
  val get_table : string -> catalog -> table option
end

module Catalog = struct
  let add_db db (Catalog dbs) = Catalog (db :: dbs)
  let delete_db db (Catalog dbs) = Catalog (List.filter (( != ) db) dbs)

  let create_db name c =
    let db = { dname = name; catalog = c; tables = [] } in
    db, add_db db c
  ;;

  let update_db db =
    let c = Database.get_catalog db in
    delete_db db c |> add_db db
  ;;

  let update_table table =
    let db = Table.get_db table in
    Database.update_table table db |> update_db
  ;;

  let create_table tname db =
    let t, db = Database.create_table tname db in
    t, update_db db
  ;;

  let create_col cname coltype table =
    let col, t = Table.create_col cname coltype table in
    col, update_table t
  ;;

  let create_cols args table =
    let t = Table.create_cols args table in
    t, update_table t
  ;;

  (* FIXME *)
  let path = "/home/arno/prog/spbu/fp/fp2022/SQL/_catalog/"
  let meta = Filename.concat path "Catalog.meta.json"
  let database_exists db (Catalog dbs) = List.mem db dbs
  let to_string catalog = Yojson.Safe.to_string (yojson_of_catalog catalog)

  let get_path_to_db = function
    | { dname } -> Filename.concat path dname
  ;;

  let get_path_to_table = function
    | { tname; db } -> Filename.concat (get_path_to_db db) tname
  ;;

  let dump (Catalog dbs as catalog) =
    if not (Sys.file_exists path) then Sys.mkdir path 0o775;
    Yojson.Safe.to_file meta (yojson_of_catalog catalog);
    List.iter (Database.dump path) dbs
  ;;

  let load meta =
    if Sys.file_exists meta
    then Some (catalog_of_yojson (Yojson.Safe.from_file meta))
    else None
  ;;

  let create () =
    match load meta with
    | Some catalog -> catalog
    | None -> Catalog []
  ;;

  let drop () =
    if Sys.file_exists path then rm_non_empty_dir path;
    create ()
  ;;

  let get_table name (Catalog dbs) = List.find_map (Database.find_table name) dbs
end

type tuple_element =
  | Int of int
  | String of string

module Tuple = struct
  type t = tuple_element array

  let from_string string_tuple hdr =
    Array.of_list
      (List.map2
         (fun string_value { cname; ctype } ->
           Format.printf "value %s %s\n" string_value cname;
           match ctype with
           | IntCol -> Int (int_of_string string_value)
           | StringCol -> String string_value)
         string_tuple
         hdr)
  ;;

  let to_string tuple =
    Array.to_list
      (Array.map
         (fun value ->
           match value with
           | Int i -> string_of_int i
           | String s -> s)
         tuple)
  ;;

  let at index tuple = Array.get tuple index
end

module Relation = struct
  type t = Tuple.t list

  let load table =
    let string_rel =
      List.map
        (fun row -> Csv.Row.to_list row)
        (Csv.Rows.load (Catalog.get_path_to_table table))
    in
    List.map
      (fun string_tuple -> Tuple.from_string string_tuple (Table.get_header table))
      string_rel
  ;;

  let to_tuple_list rel = rel
end

module Access = struct end
