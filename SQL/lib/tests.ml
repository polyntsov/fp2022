(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Meta

(*let c = Catalog.drop ()
let db, c = Catalog.create_db "main" c
let t1, c = Catalog.create_table "t1" db
let t1, c = Catalog.create_cols [ "A", IntCol; "B", StringCol ] t1
let () = Catalog.dump c
let loaded_c = Catalog.create ()

let%test _ = c = loaded_c*)

let loaded_c = Catalog.create ()

let t1 =
  let opt_t1 = Catalog.get_table "t1" loaded_c in
  match opt_t1 with
  | Some t -> t
  | None -> raise Not_found
;;

let rel = Relation.load t1
let tuples = Relation.to_tuple_list rel

let () =
  List.iter
    (fun tuple ->
      List.iter (fun s -> Format.printf "%s\t" s) (Tuple.to_string tuple);
      Format.printf "\n")
    tuples
;;
