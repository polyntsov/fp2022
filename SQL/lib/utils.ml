(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

(* TODO: use a set instead of list *)
let list_remove x = List.filter ~f:(fun a -> not (String.equal a x))

(*let free_vars =
  let rec helper acc = function
    | Var s -> s :: acc
    | Abs (s, l) -> acc @ list_remove s (helper [] l)
    | App (l, r) -> helper (helper acc r) l
  in
  helper []
;;

let is_free_in x term = List.mem (free_vars term) x ~equal:String.equal
*)

type ('a, 'b) type_error =
  { value : 'b
  ; actual : 'a
  ; expected : 'a
  }

type error =
  | UnknownTable of string
  | UnknownColumn of string
  | AmbiguousColumn of string
  | TypesMismatch of
      { x : string
      ; op : string
      ; y : string
      }

(*let var x = Var x
let abs x l = Abs (x, l)
let app l r = App (l, r)*)

(* TODO: rework this *)
module type MonadFail = sig
  include Base.Monad.S2 with type ('a, 'e) t = ('a, 'e) result

  val fail : 'e -> ('a, 'e) t
end
