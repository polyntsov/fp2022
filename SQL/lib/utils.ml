(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type ('a, 'b) type_error =
  { value : 'b
  ; actual : 'a
  ; expected : 'a
  }

type error =
  | UnknownTable of string
  | SpecifiedMoreThanOnce of string
  | AmbiguousTable of string
  | UnknownColumn of string
  | AmbiguousColumn of string
  | WrongDatabase of string
  | TypesMismatch of string
  | ParsingError of string
[@@deriving show { with_path = false }]

module type MonadFail = sig
  include Base.Monad.S2 with type ('a, 'e) t = ('a, 'e) result

  val fail : 'e -> ('a, 'e) t
end

module type Environment = sig
  val catalog_path : string
  val catalog : Meta.catalog

  (* Actually active database should not be here, but since we don't support
     switching databases at runtime let's just keep it here for simplicity *)
  val db : Meta.database
  val storage : Relation.AccessManager.storage
end

let printer_ignore show fmt expr = Format.fprintf fmt "%s" (show expr)