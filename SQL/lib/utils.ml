(** Copyright 2021-2022, Michael Polyntsov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

type ('a, 'b) type_error =
  { value : 'b
  ; actual : 'a
  ; expected : 'a
  }

type error =
  | UnknownTable of string
  | AmbiguousTable of string
  | UnknownColumn of string
  | AmbiguousColumn of string
  | TypesMismatch of
      { x : string
      ; op : string
      ; y : string
      }
  | ParsingError of string
[@@deriving show { with_path = false }]

module type MonadFail = sig
  include Base.Monad.S2 with type ('a, 'e) t = ('a, 'e) result

  val fail : 'e -> ('a, 'e) t
end
