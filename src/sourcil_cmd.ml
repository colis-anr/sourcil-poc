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

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [filename] ->
     (
       try
         ignore (Sourcil.parse_file filename);
         exit 0
       with
       | Morsmall.SyntaxError(_position, message) ->
          Format.eprintf "syntax error: %s@." message;
          exit 2
       | Sourcil.NotSupported(message) ->
          Format.printf "non-supported feature: %s@." message;
          exit 3
     )
  | _ ->
     Format.printf "Expected only one file to parse.@.";
     exit 1
