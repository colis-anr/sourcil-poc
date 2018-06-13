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
open Errors

let rec collect_redirections = function
  | Redirection (command, descr, kind, file) ->
     let (redirections, command) = collect_redirections command in
     ((descr, kind, file) :: redirections, command)
  | _ as command ->
     ([], command)

let word__to__name = function
  | [Literal l] -> l
  | _ -> raise (NotSupported ("name other than literal"))

let word__to__literal word =
  word__to__name word (*FIXME*)

let word__to__expression word =
  AST.Literal (word__to__literal word) (*FIXME*)

let simple__to__call = function
  | Simple ([], []) ->
     assert false
  | Simple ([], word :: words) ->
     (word__to__name word, List.map (fun word -> word__to__expression word) words)
  | Simple _ ->
     raise (NotSupported ("local assignments are not supported in simple command"))

  | _ -> assert false

(* Morsmall.AST.command -> Sourcil.AST.condition *)

let commands_with_no_output =
  (* Here, we can safely add any command that has "STDOUT: Not used" in the documentation. *)
  [ "[" ; "test" ; "true" ; "false" ]

let rec command__to__condition ?(redirected=false) = function

  | Simple _ as simple ->
     (* Not simple command: the words may not have subshells in them. *)
     let (name, expressions) = simple__to__call simple in
     if redirected then
       AST.CCall (name, expressions)
     else
       (
         if List.mem name commands_with_no_output then
           AST.CCall (name, expressions)
         else
           raise (NotSupported (Format.sprintf "command '%s' in conditions with no redirection of their output towards /dev/null" name))
       )

  | Async _ -> raise (NotSupported ("& in conditions"))
  | Seq _ -> raise (NotSupported ("sequence in conditions"))

  | And (first, second) ->
     AST.CAnd (command__to__condition first,
               command__to__condition second)
  | Or (first, second) ->
     AST.COr (command__to__condition first,
              command__to__condition second)
  | Not command ->
     AST.CNot (command__to__condition command)

  | Pipe _ -> raise (NotSupported ("pipe in conditions"))
  | Subshell _ -> raise (NotSupported ("subshell in conditions"))
  | For _ -> raise (NotSupported ("for in conditions"))
  | Case _ -> raise (NotSupported ("case in conditions"))
  | If _ -> raise (NotSupported ("if in conditions"))
  | While _ -> raise (NotSupported ("while in conditions"))
  | Until _ -> raise (NotSupported ("until in conditions"))
  | Function _ -> raise (NotSupported ("function in conditions"))

  | Redirection _ as command ->
     let (redirections, command) = collect_redirections command in
     (* Redirections are accepted if there is a >/dev/null *)
     if
       List.exists
         (function (1, Output, [Literal "/dev/null"]) -> true
                 | _ -> false)
         redirections
       &&
         List.for_all
           (function (1, Output, [Literal "/dev/null"]) | (_, OutputDuplicate, [Literal "1"]) -> true
                     | _ -> false)
           redirections
     then
       command__to__condition ~redirected:true command
     else
       raise (NotSupported ("redirections without >/dev/null in conditions"))

  | HereDocument _ -> raise (NotSupported ("here document in conditions"))

(* Morsmall.AST.command -> Sourcil.AST.statement *)

let rec command__to__statement = function

  | Simple _ as simple ->
     AST.Call (simple__to__call simple)

  | Async _ -> raise (NotSupported ("the asynchronous separator & is not supported"))

  | Seq (first, second) ->
     AST.Seq (command__to__statement first, command__to__statement second)

  | And _ ->
     (* note: no easy translation to 'if' *)
     raise (NotSupported ("and"))

  | Or (first, second) ->
     AST.If (command__to__condition first,
             AST.Call ("true", []),
             command__to__statement second)

  | Not _ -> raise (NotSupported ("not"))
  | Pipe _ -> raise (NotSupported ("pipe"))
  | Subshell _ -> raise (NotSupported ("subshell"))
  | For _ -> raise (NotSupported ("for"))
  | Case _ -> raise (NotSupported ("case"))

  | If (test, body, rest) ->
     AST.If (command__to__condition test,
             command__to__statement body,
             match rest with
             | None -> AST.Call ("true", [])
             | Some rest -> command__to__statement rest)

  | While _ -> raise (NotSupported ("while"))
  | Until _ -> raise (NotSupported ("until"))
  | Function _ -> raise (NotSupported ("function"))
  | Redirection _ -> raise (NotSupported ("redirection"))
  | HereDocument _ -> raise (NotSupported ("here document"))
