/* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2019 Hendrik Tews
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
 */

/** Ocamlyacc grammar for Coq existential variables info for Coq >= 8.10 */

/* In PR 10489 the dependent evar line looks like
 *
 *     (dependent evars: ; in current goal:)
 *
 * or
 *
 *     (dependent evars: ?X4:?P, ?X5 using ?X10 ?X11, ?X10 using ?X11, ?X11:?Goal1; in current goal: ?X4 ?X5 ?X10 ?X11)
 */


%{
open Evar_types
%}

%token Paren_open
%token Paren_close
%token Comma
%token Semicolon
%token Colon
%token Dependent_evars
%token Using
%token In_current_goal
%token <string> Evar

%type < Evar_types.evar_info > inst_evar_info open_evar_info evar_info
%type < Evar_types.evar_info list > evar_info_list
%type < (Evar_types.evar_info list * string list) > coq_evar_info
%start coq_evar_info
%%

coq_evar_info: Paren_open Dependent_evars evar_info_list Semicolon
                 In_current_goal evar_list Paren_close
                 { ($3, $6) }

evar_info_list:
					{ [] }
  | evar_info				{ [ $1 ] }
  | evar_info_list Comma evar_info	{ $3 :: $1 }

evar_info:
  | open_evar_info		{ $1 }
  | inst_evar_info		{ $1 }

open_evar_info: Evar Colon Evar	{ Noninstantiated($1, $3) }

inst_evar_info: Evar Using evar_list	{ Instantiated($1, $3) }

evar_list:
					{ [] }
  | evar_list Evar			{ $2 :: $1 }

%%
