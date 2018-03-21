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

let parse_files =
  List.iter
    (fun filename ->
      Format.printf "@[<h 2>Parsing file \"%s\"... @?" filename;
      (
        try
          let ast = Sourcil.parse_file filename in
          Format.printf "succeeded.@\n  %a" Sourcil.pp_print_debug ast
        with
        | Morsmall.SyntaxError(_position, message) ->
           Format.printf "failed because of a syntax error:@\n  %s" message
        | Sourcil.NotSupported(_position, message) ->
           Format.printf "failed because of a non-supported feature:\n  %s" message
      );
      Format.printf "@]@.")

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
     Format.eprintf "No file to parse!@.";
     exit 1
  | files ->
     parse_files files
