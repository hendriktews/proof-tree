(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011, 2012 Hendrik Tews
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
 * 
 * $Id: about_window.ml,v 1.4 2012/01/03 09:40:43 tews Exp $
 *)


(** Creation and display of the about window *)


let about_window = ref None

let delete_about () =
  match !about_window with
    | None -> ()
    | Some about ->
      about#destroy ();
      about_window := None


let about_button = function
  | `CANCEL -> delete_about ()
  | _ -> ()


let about_comment =
  "Prooftree displays proof trees for Coq under control of \
   the Proof General user interface. Prooftree has been developed by \
   Hendrik Tews and is published under GPL version 3. For more information \
   visit http://askra.de/software/prooftree/."

let show_about_window () =
  match !about_window with
    | Some about -> about#present ()
    | None ->
      let about = 
	GWindow.about_dialog 
	  ~name:"Prooftree"
	  ~comments:about_comment
	  ~copyright:("Prooftree version " ^ Version.version 
		      ^ " © Hendrik Tews")
	  (* ~website_label:string -> *)
	  (* ~parent:#window_skel -> *)
	  (* ~destroy_with_parent:bool -> *)
	  () 
      in
      about_window := Some about;
      ignore(about#connect#destroy ~callback:delete_about);
      ignore(about#connect#response ~callback:about_button);
      about#show ()
