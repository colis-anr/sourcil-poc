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

type split = bool                                              [@@deriving show]

type return_code =
  | Success
  | Failure_
  | Previous                                                   [@@deriving show]
           
type expression_component =
  | ELiteral of string
  | EVariable of split * name
  | ESubshell of split * statement

and expression = expression_component list

and pattern_component =
  | PLiteral of string
  | PAll

and pattern =
  pattern_component list

and statement =
  | Assign of name * expression
  | Seq of statement * statement
  | Subshell of statement
  | If of statement * statement * statement
  | Pipe of statement * statement
  | While of statement * statement
  | Case of expression * case_item list
  | Ignore of statement
  | Foreach of name * literal list * statement
  | Not of statement

  | Call of name * expression list
  | CallFunction of name * expression list

  | EnterStrictMode (* set -e *)
  | LeaveStrictMode (* set +e *)
  | Exit_ of return_code
  | Return of return_code
           
and case_item =
  pattern * statement

[@@deriving show]
