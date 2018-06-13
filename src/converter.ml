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

let rec collect_redirections = function
  | Redirection { command ; descr ; kind ; file } ->
     let (redirections, command) = collect_redirections command.value in
     ((descr, kind, file) :: redirections, command)
  | _ as command ->
     ([], command)

let word__to__name word =
  String.iter
    (fun c ->
      if List.mem c ['`'; '*'; '?'; '('; '{'] then
        raise (NotSupported (dummy_position, "forbidden character: `"^(String.make 1 c)^"'")))
    word;
  word (*FIXME*)

let word__to__literal word =
  word__to__name word (*FIXME*)

let word__to__expression word =
  AST.Literal (word__to__literal word) (*FIXME*)

let simple__to__call = function

  | Simple { assignments = [] ; words = [] } ->
     assert false
  | Simple { assignments = [] ; words = word :: words } ->
     AST.{ name = word__to__name word.value ;
           arguments = List.map (fun word -> word__to__expression word.value) words }
  | Simple _ ->
     raise (NotSupported (dummy_position, "local assignments are not supported in simple command"))

  | _ -> assert false

(* Morsmall.AST.command -> Sourcil.AST.condition *)

let commands_with_no_output =
  (* Here, we can safely add any command that has "STDOUT: Not used" in the documentation. *)
  [ "[" ; "test" ; "true" ; "false" ]

let rec command__to__condition ?(redirected=false) = function

  | Simple _ as simple ->
     (* Not simple command: the words may not have subshells in them. *)
     let call = simple__to__call simple in
     if redirected then
       AST.CCall call
     else
       (
         if List.mem call.AST.name commands_with_no_output then
           AST.CCall call
         else
           raise (NotSupported (dummy_position, Format.sprintf "command '%s' in conditions with no redirection of their output towards /dev/null" call.AST.name))
       )

  | Async _ -> raise (NotSupported (dummy_position, "& in conditions"))
  | Seq _ -> raise (NotSupported (dummy_position, "sequence in conditions"))

  | And (first, second) ->
     AST.CAnd (command__to__condition first.value,
               command__to__condition second.value)
  | Or (first, second) ->
     AST.COr (command__to__condition first.value,
              command__to__condition second.value)
  | Not command ->
     AST.CNot (command__to__condition command.value)

  | Pipe _ -> raise (NotSupported (dummy_position, "pipe in conditions"))
  | Subshell _ -> raise (NotSupported (dummy_position, "subshell in conditions"))
  | For _ -> raise (NotSupported (dummy_position, "for in conditions"))
  | Case _ -> raise (NotSupported (dummy_position, "case in conditions"))
  | If _ -> raise (NotSupported (dummy_position, "if in conditions"))
  | While _ -> raise (NotSupported (dummy_position, "while in conditions"))
  | Until _ -> raise (NotSupported (dummy_position, "until in conditions"))
  | Function _ -> raise (NotSupported (dummy_position, "function in conditions"))

  | Redirection _ as command ->
     let (redirections, command) = collect_redirections command in
     (* Redirections are accepted if there is a >/dev/null *)
     if
       List.exists
         (function (1, Output, "/dev/null") -> true
                 | _ -> false)
         redirections
       &&
         List.for_all
           (function (1, Output, "/dev/null") | (_, OutputDuplicate, "1") -> true
                     | _ -> false)
           redirections
     then
       command__to__condition ~redirected:true command
     else
       raise (NotSupported (dummy_position, "redirections without >/dev/null in conditions"))

  | HereDocument _ -> raise (NotSupported (dummy_position, "here document in conditions"))

(* Morsmall.AST.command -> Sourcil.AST.statement *)

let rec command__to__statement = function

  | Simple _ as simple ->
     AST.Call (simple__to__call simple)

  | Async _ -> raise (NotSupported (dummy_position, "the asynchronous separator & is not supported"))

  | Seq (first, second) ->
     AST.Seq
       { first = command__to__statement first.value ;
         second = command__to__statement second.value }

  | And _ ->
     (* note: no easy translation to 'if' *)
     raise (NotSupported (dummy_position, "and"))

  | Or (first, second) ->
     AST.If {
         test = command__to__condition first.value ;
         body = AST.(Call { name = "true" ; arguments = [] }) ;
         rest = command__to__statement second.value
       }

  | Not _ -> raise (NotSupported (dummy_position, "not"))
  | Pipe _ -> raise (NotSupported (dummy_position, "pipe"))
  | Subshell _ -> raise (NotSupported (dummy_position, "subshell"))
  | For _ -> raise (NotSupported (dummy_position, "for"))
  | Case _ -> raise (NotSupported (dummy_position, "case"))

  | If if_clause ->
     AST.If {
         test = command__to__condition if_clause.test.value ;
         body = command__to__statement if_clause.body.value ;
         rest =
           match if_clause.rest with
           | None -> AST.(Call { name = "true" ; arguments = [] })
           | Some rest -> command__to__statement rest.value
       }

  | While _ -> raise (NotSupported (dummy_position, "while"))
  | Until _ -> raise (NotSupported (dummy_position, "until"))
  | Function _ -> raise (NotSupported (dummy_position, "function"))
  | Redirection _ -> raise (NotSupported (dummy_position, "redirection"))
  | HereDocument _ -> raise (NotSupported (dummy_position, "here document"))
