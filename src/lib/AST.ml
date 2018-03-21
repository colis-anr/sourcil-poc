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

type expression = string (*FIXME*)                             [@@deriving show]

type call =
  { name : name ;
    arguments : literal list }                                 [@@deriving show]

type condition = call (*FIXME*)                                [@@deriving show]

type statement =

  | Assign of
      { variable : name ;
        expression : expression }

  | Seq of
      { first : statement ;
        second : statement }

  | Call of call

  | If of
      { test : condition ;
        body : statement ;
        rest : statement }

  | Foreach of
      { variable : name ;
        literals : literal list ;
        body : statement }

[@@deriving show]

type statement_list = statement list                           [@@deriving show]
