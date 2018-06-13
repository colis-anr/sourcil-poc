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
   
let commands_with_no_output =
  (* Here, we can safely add any command that has "STDOUT: Not used" in the documentation. *)
  [ "[" ; "test" ; "true" ; "false" ]

let rec collect_redirections = function
  | Redirection (command, descr, kind, file) ->
     let (redirections, command) = collect_redirections command in
     ((descr, kind, file) :: redirections, command)
  | _ as command ->
     ([], command)

and word__to__name = function
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

and word_component_double_quoted__to__expression_component = function
  | Name n -> ELiteral n
  | Literal l -> ELiteral l
  | Variable v -> EVariable v
  | Subshell c -> ESubshell (command_list__to__statement_list c)

  | DoubleQuoted _ -> assert false
  | GlobAll -> assert false
  | GlobAny -> assert false
  | GlobRange _ -> assert false
  | Assignment _ -> assert false

and word_double_quoted__to__expression word =
  List.map word_component_double_quoted__to__expression_component word

and word_component__to__expression = function
  | Name n -> [AST.ELiteral n]
  | Literal l -> [AST.ELiteral l]
  | Variable v -> [AST.ESplitVariable v]
  | DoubleQuoted w -> word_double_quoted__to__expression w
  | Subshell c -> [AST.ESplitSubshell (command_list__to__statement_list c)]

  | Assignment _ -> raise (NotSupported "assignment")
  | GlobAll -> raise (NotSupported "glob *")
  | GlobAny -> raise (NotSupported "glob ?")
  | GlobRange _ -> raise (NotSupported "char range")

and word__to__expression word =
  List.map word_component__to__expression word
  |> List.flatten

and word__to__pattern_component = function
  | [Literal l] -> PLiteral l
  | _ -> raise (NotSupported "pattern other than literal")
  
and pattern__to__pattern pattern =
  List.map word__to__pattern_component pattern
  
and simple__to__call = function
  | Simple ([], []) ->
     assert false
  | Simple ([], word :: words) ->
     (word__to__name word, List.map (fun word -> word__to__expression word) words)
  | Simple _ ->
     raise (NotSupported ("local assignments are not supported in simple command"))

  | _ -> assert false

(* Morsmall.AST.command -> Sourcil.AST.condition *)

and command__to__condition ?(redirected=false) = function

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
  | Redirection _ -> raise (NotSupported ("redirection"))
  | HereDocument _ -> raise (NotSupported ("here document"))

and case_item__to__case_item = function
  | (pattern, Some command) ->
     (pattern__to__pattern pattern, command__to__statement command)
  | (_, None) ->
     raise (NotSupported ("case item with empty command"))

and command_list__to__statement_list cl =
  List.map command__to__statement cl
