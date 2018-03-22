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

open Morsmall.AST
open Morsmall.Location
open Errors

let word__to__name word = word (*FIXME*)
let word__to__literal word = word (*FIXME*)
let word__to__expression word = word (*FIXME*)

let simple__to__call = function

  | Simple { assignments = [] ; words = [] } ->
     assert false
  | Simple { assignments = [] ; words = word :: words } ->
     AST.{ name = word__to__name word.value ;
           arguments = List.map (fun word -> word__to__expression word.value) words }
  | Simple _ ->
     raise (NotSupported (dummy_position, "local assignments are not supported in simple command"))

  | _ -> assert false

let command__to__condition = function

  | Simple _ as simple ->
     simple__to__call simple

  | _ ->
     raise (NotSupported (dummy_position, "nothing supported in conditions but simple commands"))

let rec command__to__statement = function

  | Simple _ as simple ->
     AST.Call (simple__to__call simple)

  | Async _ ->
     raise (NotSupported (dummy_position, "the asynchronous separator & is not supported"))

  | Seq (first, second) ->
     AST.Seq
       { first = command__to__statement first.value ;
         second = command__to__statement second.value }

  | And _ ->
     raise (NotSupported (dummy_position, "and"))

  | Or _ ->
     raise (NotSupported (dummy_position, "or"))

  | Not _ ->
     raise (NotSupported (dummy_position, "not"))

  | Pipe _ ->
     raise (NotSupported (dummy_position, "pipe"))

  | Subshell _ ->
     raise (NotSupported (dummy_position, "subshell"))

  | For _ ->
     raise (NotSupported (dummy_position, "for"))

  | Case _ ->
     raise (NotSupported (dummy_position, "case"))

  | If if_clause ->
     AST.If {
         test = command__to__condition if_clause.test.value ;
         body = command__to__statement if_clause.body.value ;
         rest =
           match if_clause.rest with
           | None -> AST.(Call { name = "true" ; arguments = [] })
           | Some rest -> command__to__statement rest.value
       }

  | While _ ->
     raise (NotSupported (dummy_position, "while"))

  | Until _ ->
     raise (NotSupported (dummy_position, "until"))

  | Function _ ->
     raise (NotSupported (dummy_position, "function"))

  | Redirection _ ->
     raise (NotSupported (dummy_position, "redirection"))

  | HereDocument _ ->
     raise (NotSupported (dummy_position, "here document"))
