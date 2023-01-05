(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type column_type =
  | IntCol
  | StringCol
[@@deriving yojson]

type column =
  { cname : string
  ; ctype : column_type
  }
[@@deriving yojson]

type header = column list [@@deriving yojson]

type table =
  { tname : string
  ; header : header
  }
[@@deriving yojson]

type database =
  { dname : string
  ; tables : table list
  }
[@@deriving yojson]

type catalog =
  { dbs : database list
  ; cpath : string
  }
[@@deriving yojson]

let column_type_to_string = function
  | IntCol -> "IntCol"
  | StringCol -> "StringCol"
;;

let column_type_of_string = function
  | "IntCol" -> IntCol
  | "StringCol" -> StringCol
  | _ -> raise (Failure "Unkown column type")
;;

let file_perm = 0o775

module Table = struct
  let create_col cname coltype ({ header } as table) =
    let col = { cname; ctype = coltype } in
    col, { table with header = col :: header }
  ;;

  let create_cols args table =
    List.fold_right
      (fun (cname, coltype) table -> snd (create_col cname coltype table))
      args
      table
  ;;

  let get_name { tname } = tname
  let get_header { header } = header

  let get_column_names table =
    get_header table |> fun cols -> List.map (fun { cname } -> cname) cols
  ;;

  let add_column col ({ header = cols } as t) = { t with header = col :: cols }
  let add_columns cols table = List.fold_right add_column cols table
  let get_column t index = List.nth (get_header t) index
end

module Database = struct
  let get_name { dname } = dname
  let table_exists table { tables } = List.mem table tables

  let load path =
    let load_table tpath = Csv.load tpath in
    load_table path
  ;;

  let add_table table ({ tables } as db) = { db with tables = table :: tables }

  let delete_table table { dname; tables } =
    { dname; tables = List.filter (( != ) table) tables }
  ;;

  let update_table old_table new_table db =
    delete_table old_table db |> add_table new_table
  ;;

  let create_table tname db =
    let t = { tname; header = [] } in
    t, add_table t db
  ;;

  let dump path db = Sys.mkdir (Filename.concat path (get_name db)) file_perm

  let find_table ?(ci = false) name { tables } =
    let op = if ci then Base.String.Caseless.( = ) else ( = ) in
    List.find_opt (fun { tname } -> op name tname) tables
  ;;
end

let rec rm_non_empty_dir path =
  match Sys.is_directory path with
  | true ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_non_empty_dir (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path
;;

module Catalog = struct
  let add_db db ({ dbs = old_dbs } as c) = { c with dbs = db :: old_dbs }

  let delete_db db ({ dbs = old_dbs } as c) =
    { c with dbs = List.filter (( != ) db) old_dbs }
  ;;

  let create_db name c =
    let db = { dname = name; tables = [] } in
    db, add_db db c
  ;;

  let find_table_db table { dbs } = List.find (Database.table_exists table) dbs
  let update_db old_db new_db c = delete_db old_db c |> add_db new_db

  let update_table old_table new_table c =
    let old_db = find_table_db old_table c in
    let new_db = Database.update_table old_table new_table old_db in
    update_db old_db new_db c
  ;;

  let create_table tname old_db c =
    let t, new_db = Database.create_table tname old_db in
    t, update_db old_db new_db c
  ;;

  let create_col cname coltype old_table c =
    let col, new_table = Table.create_col cname coltype old_table in
    col, update_table old_table new_table c
  ;;

  let create_cols args old_table c =
    let new_table = Table.create_cols args old_table in
    new_table, update_table old_table new_table c
  ;;

  let catalog_dir = "_catalog"
  let get_catalog_dir path = Filename.concat path catalog_dir
  let get_path { cpath } = cpath

  let meta path =
    assert (Filename.check_suffix path catalog_dir);
    Filename.concat path "Catalog.meta.json"
  ;;

  let database_exists db { dbs } = List.mem db dbs
  let to_string catalog = Yojson.Safe.to_string (yojson_of_catalog catalog)
  let get_path_to_db { dname } c = Filename.concat (get_path c) dname
  let catalog_exists c = Sys.file_exists (get_path c)

  let create path =
    let cpath = get_catalog_dir path in
    if Sys.file_exists cpath
    then raise (Failure "catalog already exists")
    else { dbs = []; cpath }
  ;;

  let load path =
    let cpath = get_catalog_dir path in
    let meta_path = meta cpath in
    if Sys.file_exists meta_path
    then (
      try catalog_of_yojson (Yojson.Safe.from_file meta_path) with
      | _ -> raise (Failure (Format.sprintf "Meta file %s has wrong format." meta_path)))
    else raise (Failure "Meta does not exist")
  ;;

  let init path =
    let cpath = get_catalog_dir path in
    let meta_path = meta cpath in
    if not (Sys.file_exists meta_path) then create path else load path
  ;;

  let recreate path =
    let cpath = get_catalog_dir path in
    if Sys.file_exists cpath then rm_non_empty_dir cpath;
    create path
  ;;

  let dump ({ dbs; cpath } as catalog) =
    if not (catalog_exists catalog) then Sys.mkdir cpath file_perm;
    Yojson.Safe.to_file (meta cpath) (yojson_of_catalog catalog);
    List.iter (Database.dump cpath) dbs
  ;;

  let drop ({ cpath } as c) = if catalog_exists c then rm_non_empty_dir cpath
  let get_db name { dbs } = List.find_opt (fun { dname } -> dname = name) dbs
  let get_table_ci name { dbs } = List.filter_map (Database.find_table ~ci:true name) dbs

  let get_table_path ({ tname } as t) c =
    let db_path = get_path_to_db (find_table_db t c) c in
    Filename.concat db_path tname
  ;;

  let get_dbs { dbs } = dbs
  let get_tables { tables } = tables
  let get_table { tables } name = List.find_opt (fun { tname } -> tname = name) tables
  let get_table_types table = List.map (fun { ctype } -> ctype) (Table.get_header table)
  let get_db name c = List.find_opt (fun db -> Database.get_name db = name) (get_dbs c)
end
