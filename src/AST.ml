(******************************************************************************)
(*                                                                            *)
(*                                  SourCIL                                   *)
(*              Utilities around the CoLiS Intermediate Language              *)
(*                                                                            *)
(*   Copyright (C) 2018  Yann Régis-Gianas, Ralf Treinen, Nicolas Jeannerod   *)
(*                                                                            *)
(*   This program is free software: you can redistribute it and/or modify     *)
(*   it under the terms of the GNU General Public License as published by     *)
(*   the Free Software Foundation, either version 3 of the License, or        *)
(*   (at your option) any later version.                                      *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU General Public License for more details.                             *)
(*                                                                            *)
(*   You should have received a copy of the GNU General Public License        *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                            *)
(******************************************************************************)

type name = string                                             [@@deriving show]
type literal = string                                          [@@deriving show]

type expression_component =
  | ELiteral of string
  | EVariable of name
  | ESplitVariable of name
  | ESubshell of statement_list
  | ESplitSubshell of statement_list                            [@@deriving show]

and expression = expression_component list                                [@@deriving show]

and pattern_component =
  | PLiteral of string

and pattern =
  pattern_component list
             
and call = name * expression list                              [@@deriving show]

and condition =
  | CCall of call
  | CAnd of condition * condition
  | COr of condition * condition
  | CNot of condition                                          [@@deriving show]

and statement =
  | SAssign of name * expression
  | SSeq of statement * statement
  | SCall of call (*FIXME: Call, CallFunction, CallBuiltin?*)
  | SSubshell of statement_list
  | SIf of condition * statement * statement
  | SPipe of statement * statement
  | SWhile of condition * statement
  | SCase of expression * case_item list
  | SForeach of name * literal list * statement                 [@@deriving show]

and case_item =
  pattern * statement

and statement_list = statement list                            [@@deriving show]
