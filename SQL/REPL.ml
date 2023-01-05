open Base
open Sql_lib

let run_repl _ =
  Caml.Format.eprintf "OCaml-style toplevel (ocamlc, utop) is not implemented"
;;

let run_single e =
  let query = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  match Interpret.interpret query e with
  | Result.Error error -> Caml.Format.printf "%s\n%!" (Utils.show_error error)
  | Result.Ok rel -> Csv.print_readable (Relation.to_csv rel)
;;

type opts =
  { mutable batch : bool
  ; mutable catalog_path : string
  ; mutable dbname : string option
  ; mutable make_db_from : string option
  }

let () =
  let opts =
    { batch = false
    ; catalog_path = Filename.current_dir_name
    ; make_db_from = None
    ; dbname = None
    }
  in
  let open Caml.Arg in
  parse
    [ ( "-no-repl"
      , Unit (fun () -> opts.batch <- true)
      , "Read from stdin single program, instead of running full REPL" )
    ; ( "-catalog_path"
      , String (fun path -> opts.catalog_path <- path)
      , "Path to the directory where to store DB data. Defaults to current dir" )
    ; "-db", String (fun dbname -> opts.dbname <- Some dbname), "Database to use"
    ; ( "-make_db_from"
      , String (fun path -> opts.make_db_from <- Some path)
      , "Create database from specified directory and exit" )
    ]
    (fun arg ->
      Caml.Format.eprintf "Unrecognized argument '%s', try --help\n" arg;
      Caml.exit 1)
    "Read-Eval-Print-Loop for mini SQL language.\n\tusage: /REPL.exe -db=<dbname>";
  try
    let catalog_path = opts.catalog_path in
    let catalog = Meta.Catalog.init catalog_path in
    match opts.make_db_from with
    | None ->
      let module Env : Utils.Environment = struct
        let catalog_path = catalog_path
        let catalog = catalog

        let db =
          let storage =
            let dbname =
              match opts.dbname with
              | None ->
                Format.eprintf
                  "Changing database via repl commands is not supported yet, please \
                   specify it using '-db' command line option";
                Caml.exit 1
              | Some dbname -> dbname
            in
            match Meta.Catalog.get_db dbname catalog with
            | None ->
              Format.eprintf "No database named %s" dbname;
              Caml.exit 1
            | Some db -> Relation.AccessManager.set_active db None catalog
          in
          Relation.AccessManager.get_active_db storage
        ;;
      end
      in
      Format.printf "Connected to %s\n%!" (Meta.Database.get_name Env.db);
      if opts.batch then run_single (module Env) else run_repl ()
    | Some path ->
      let _c = Relation.AccessManager.make_db_from path catalog in
      ()
  with
  | Failure s -> Format.eprintf "An error occured: %s\n%!" s
;;
