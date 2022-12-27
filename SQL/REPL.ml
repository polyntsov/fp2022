open Base
open Sql_lib

let run_repl _ =
  Caml.Format.eprintf "OCaml-style toplevel (ocamlc, utop) is not implemented"
;;

(*let run_single eval =
  let text = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  let ast = Parser.parse text in
  match ast with
  | Error e -> Caml.Format.printf "Error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    Caml.Format.printf "Parsed result: %a\n%!" Printast.pp_named ast;
    (match eval ast with
     | rez -> Caml.Format.printf "Evaluated result: %a\n%!" Printast.pp_named rez)
;;

type strategy =
  | CBN
  | CBV
  | NO
  | AO

type opts =
  { mutable batch : bool
  ; mutable stra : strategy
  }*)
