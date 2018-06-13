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
open AST

let redirection_as_ignore command =
  let rec redirection_as_ignore_aux todevnull = function
    | Redirection (command, 1, Output, [Literal "/dev/null"]) ->
       redirection_as_ignore_aux true command
    | Redirection (command, _, OutputDuplicate, [Literal "1"]) ->
       redirection_as_ignore_aux todevnull command
    | Redirection _ -> None
    | _ as command ->
       if todevnull then
         Some command
       else
         None
  in
  redirection_as_ignore_aux false command

let rec word__to__name = function
  | [Name l] -> l (* FIXME: we probably want to exclude characters here *)
  | [Literal l] -> l (* ? *)
  | [DoubleQuoted _] -> raise (NotSupported "double quotes in name")
  | [Variable _] -> raise (NotSupported "variable in name")
  | [Subshell _] -> raise (NotSupported "subshell in name")
  | [Assignment _] -> raise (NotSupported "assignment in name")
  | [GlobAll] -> raise (NotSupported "glob * in name")
  | [GlobAny] -> raise (NotSupported "glob ? in name")
  | [GlobRange _] -> raise (NotSupported "glob range in name")
  | [] -> raise (NotSupported "empty name")
  | _ :: _ :: _ -> raise (NotSupported "name >=2")

and word__to__literal = function
  | [Literal l] -> l
  | _ -> raise (NotSupported "literal other than literal")

and word_component_double_quoted__to__expression_component ?(pure=false) = function
  | Name n -> ELiteral n
  | Literal l -> ELiteral l
  | Variable v -> EVariable v
  | Subshell c when not pure -> ESubshell (command_list__to__statement_list c)

  | Subshell _ -> raise (NotSupported "subshell in pure expression")
  | DoubleQuoted _ -> assert false
  | GlobAll -> assert false
  | GlobAny -> assert false
  | GlobRange _ -> assert false
  | Assignment _ -> assert false

and word_double_quoted__to__expression ?(pure=false) word =
  List.map (word_component_double_quoted__to__expression_component ~pure) word

and word_component__to__expression ?(pure=false) = function
  | Name n ->
     [AST.ELiteral n]
  | Literal l ->
     [AST.ELiteral l]
  | Variable v ->
     [AST.ESplitVariable v]
  | DoubleQuoted w ->
     word_double_quoted__to__expression ~pure w
  | Subshell c when not pure ->
     [AST.ESplitSubshell (command_list__to__statement_list c)]

  | Subshell _ -> raise (NotSupported "subshell in pure expression")
  | Assignment _ -> raise (NotSupported "assignment")
  | GlobAll -> raise (NotSupported "glob *")
  | GlobAny -> raise (NotSupported "glob ?")
  | GlobRange _ -> raise (NotSupported "char range")

and word__to__expression ?(pure=false) word =
  List.map (word_component__to__expression ~pure) word
  |> List.flatten

and word__to__pattern_component = function
  | [Literal l] -> PLiteral l
  | _ -> raise (NotSupported "pattern other than literal")

and pattern__to__pattern pattern =
  List.map word__to__pattern_component pattern

and simple__to__call ?(pure=false) = function
  | Simple ([], []) ->
     assert false
  | Simple ([], word :: words) ->
     (word__to__name word,
      List.map (word__to__expression ~pure) words)
  | Simple _ ->
     raise (NotSupported ("local assignments are not supported in simple command"))

  | _ -> assert false

(* Morsmall.AST.command -> Sourcil.AST.condition *)

and command__to__condition = function
  | Simple _ as simple ->
     CCall (simple__to__call ~pure:true simple)

  | Async _ -> raise (NotSupported ("& in conditions"))
  | Seq _ -> raise (NotSupported ("sequence in conditions"))

  | And (first, second) ->
     CAnd (command__to__condition first,
           command__to__condition second)
  | Or (first, second) ->
     COr (command__to__condition first,
          command__to__condition second)
  | Not command ->
     CNot (command__to__condition command)

  | Pipe _ -> raise (NotSupported ("pipe in conditions"))
  | Subshell _ -> raise (NotSupported ("subshell in conditions"))
  | For _ -> raise (NotSupported ("for in conditions"))
  | Case _ -> raise (NotSupported ("case in conditions"))
  | If _ -> raise (NotSupported ("if in conditions"))
  | While _ -> raise (NotSupported ("while in conditions"))
  | Until _ -> raise (NotSupported ("until in conditions"))
  | Function _ -> raise (NotSupported ("function in conditions"))

  | Redirection _ as command ->
     (
       match redirection_as_ignore command with
       | None -> raise (NotSupported ("other redirections in conditions"))
       | Some command -> CIgnore (command__to__condition command)
     )

  | HereDocument _ ->
     raise (NotSupported ("here document in conditions"))

(* Morsmall.AST.command -> Sourcil.AST.statement *)

and command__to__statement = function

  | Simple _ as simple ->
     SCall (simple__to__call simple)

  | Async _ ->
     raise (NotSupported ("the asynchronous separator & is not supported"))

  | Seq (first, second) ->
     SSeq (command__to__statement first, command__to__statement second)

  | And _ ->
     (* note: no easy translation to 'if' *)
     raise (NotSupported ("and"))

  | Or (first, second) ->
     SIf (command__to__condition first,
          SCall ("true", []),
          command__to__statement second)

  | Not _ ->
     (* no easy translation to 'if' *)
     raise (NotSupported ("not"))

  | Pipe (first, second) ->
     SPipe (command__to__statement first,
            command__to__statement second)

  | Subshell command ->
     SSubshell (command_list__to__statement_list [command]) (*FIXME*)

  | For (_, None, _) ->
     raise (NotSupported "for with no list")

  | For (name, Some literals, command) ->
     SForeach (name,
               List.map word__to__literal literals,
               command__to__statement command)

  | Case (w, cil) ->
     SCase (word__to__expression w,
            List.map case_item__to__case_item cil)

  | If (test, body, rest) ->
     SIf (command__to__condition test,
          command__to__statement body,
          match rest with
          | None -> SCall ("true", [])
          | Some rest -> command__to__statement rest)

  | While (cond, body) ->
     SWhile (command__to__condition cond,
             command__to__statement body)

  | Until (cond, body) ->
     SWhile (CNot (command__to__condition cond),
             command__to__statement body)

  | Function _ -> raise (NotSupported ("function"))

  | Redirection _ as command ->
     (
       match redirection_as_ignore command with
       | None -> raise (NotSupported ("other redirections"))
       | Some command ->
          SIgnore (command__to__statement command)
     )

  | HereDocument _ ->
     raise (NotSupported ("here document"))

and case_item__to__case_item = function
  | (pattern, Some command) ->
     (pattern__to__pattern pattern, command__to__statement command)
  | (_, None) ->
     raise (NotSupported ("case item with empty command"))

and command_list__to__statement_list cl =
  List.map command__to__statement cl
