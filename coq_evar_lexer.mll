(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2019 - 2024 Hendrik Tews
 * 
 * This file is part of "prooftree".
 * 
 * "prooftree" is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * "prooftree" is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the parent
 * directories for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with "prooftree". If not, see <http://www.gnu.org/licenses/>.
 *)

(** Ocamllex lexer for Coq existential variables info for Coq >= 8.10 *)

{
  open Coq_evar_parser
}

(* Non-ascii UTF-8 unicode has the highest bit set. We trust PG to only
 * sent valid UTF-8 and accept here all bytes with highest bit set as
 * letters.
 *)

let letter = ['A'-'Z''a'-'z''_''\x80'-'\xff']
let alphanum = ['0'-'9''A'-'Z''a'-'z''_''\'''\x80'-'\xff']

rule evar_token = parse
    '('			{ Paren_open }
  | ')'			{ Paren_close }
  | ','			{ Comma }
  | ';'			{ Semicolon }
  | ':'			{ Colon }
  | "dependent evars:"  { Dependent_evars }
  | "using"		{ Using }
  | "in current goal:"		{ In_current_goal }
  | '?' letter alphanum*	{ Evar(Lexing.lexeme lexbuf) }
  | [' ' '\t' '\n' ] +	{ evar_token lexbuf }

{ }
