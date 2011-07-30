(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 Hendrik Tews
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
 * $Id: gtk_ext.ml,v 1.9 2011/07/30 18:45:50 tews Exp $
 *)


(** Some misc LablGtk extensions *)


class better_drawable ?colormap w pc = 
object (self)
  inherit GDraw.drawable ?colormap w

  val pango_context = (pc : GPango.context_rw)
  method pango_context = pango_context    

  method get_foreground = (Gdk.GC.get_values gc).Gdk.GC.foreground
  method get_background = (Gdk.GC.get_values gc).Gdk.GC.background

end


let error_message_dialog message =
  let err = GWindow.message_dialog ~message
    ~message_type:`ERROR
    ~buttons:GWindow.Buttons.ok () 
  in
  (* ignore(err#connect#response ~callback:(fun _ -> err#destroy())); *)
  ignore(err#connect#response ~callback:(fun _ -> exit 1));
  err#show()


let round_color_2_digits co =
  min ((co + 128) / 256) 0xff

let pango_markup_bold_color s color =
  Printf.sprintf
    "<span weight=\"bold\" color=\"#%02X%02X%02X\">%s</span>"
    (round_color_2_digits (Gdk.Color.red color))
    (round_color_2_digits (Gdk.Color.green color))
    (round_color_2_digits (Gdk.Color.blue color))
    s

(* XXX why is this necessary?? *)
let realloc_color c =
  GDraw.color (`RGB((Gdk.Color.red c), (Gdk.Color.green c),(Gdk.Color.blue c)))
