(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 - 2024 Hendrik Tews
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


(** Some misc LablGtk extensions *)


(** An extension of {xref lablgtk class GDraw.drawable} with a few
    convinience methods. 
*)
class better_drawable ?colormap w pc = 
object
  inherit GDraw.drawable ?colormap w

  (** Link a writable Pango context for easy access. *)
  val pango_context = (pc : GPango.context_rw)

  (** Return a writable Pango context. *)
  method pango_context = pango_context    

  (** Return the current foreground color of the graphics context of
      this drawable. 
  *)
  method get_foreground = (Gdk.GC.get_values gc).Gdk.GC.foreground

  (** Return the current background color of the graphics context of
      this drawable. 
  *)
  method get_background = (Gdk.GC.get_values gc).Gdk.GC.background
end


(** Convinience wrapper around {xref lablgtk val
    GWindow.message_dialog}. [run_message_dialog message message_type]
    displays a modal message dialog of [message_type] with message
    [message] and one OK button. The dialog is destroyed when the OK
    button is pressed. [message_type] must be one of [`INFO],
    [`WARNING], [`QUESTION] and [`ERROR ].
*)
let run_message_dialog message message_type =
  let warn = GWindow.message_dialog ~message ~message_type
    ~buttons:GWindow.Buttons.ok ()
  in
  ignore(warn#run());
  warn#destroy()


(** Another convenience wrapper around {xref lablgtk val
    GWindow.message_dialog}. [error_message_dialog message] displays a
    modal error message dialog (of type [`ERROR]) with message
    [message] and one OK button. The application is terminated with
    exit status 1 after the error has been acknowledged.
*)
let error_message_dialog message =
  run_message_dialog message `ERROR;
  exit 1


(** Scroll the given adjustment [direction] number of pages into the
    direction idicated by the sign of [direction]. This function is
    used for scrolling with keys.
*)
let scroll_adjustment (a : GData.adjustment) direction =
  let new_val = a#value +. float_of_int(direction) *. a#step_increment in
  let new_val = if new_val < 0.0 then 0.0 else new_val in
  let max = max 0.0 (a#upper -. a#page_size) in
  let new_val = if new_val > max then max else new_val in
  a#set_value new_val


(** [inside_adj_range adjustment x] checks if [x] is inside the
    visible range of the adjustment [adjustment].
*)
let inside_adj_range adjustment x =
  let page_l = adjustment#value in
  let page_u = page_l +. adjustment#page_size in
  page_l <= x && x <= page_u

(** [range_inside_adj_range adjustment xl xh] checks if the range from
    [xl] to [xh] is inside the visible range of the adjustment
    [adjustment]. Does only produce correct results if [xl <= xh].
*)
let range_inside_adj_range adjustment xl xh =
  let page_l = adjustment#value in
  let page_u = page_l +. adjustment#page_size in
  page_l <= xl && xh <= page_u



(** Round a 16-bit color value to 8 bit. *)
let round_color_2_digits co =
  min ((co + 128) / 256) 0xff


(** [pango_markup_color s color] adds Pango markup for using color
    [color] arouns [s].
*)
let pango_markup_color s color =
  Printf.sprintf
    "<span color=\"#%02X%02X%02X\">%s</span>"
    (round_color_2_digits (Gdk.Color.red color))
    (round_color_2_digits (Gdk.Color.green color))
    (round_color_2_digits (Gdk.Color.blue color))
    s

(** [pango_markup_bold_color s color] adds Pango markup for using a
    bold font in color [color] arouns [s].
*)
let pango_markup_bold_color s color =
  Printf.sprintf
    "<span weight=\"bold\" color=\"#%02X%02X%02X\">%s</span>"
    (round_color_2_digits (Gdk.Color.red color))
    (round_color_2_digits (Gdk.Color.green color))
    (round_color_2_digits (Gdk.Color.blue color))
    s

(* XXX why is this necessary?? *)
(** Reallocate a Gdk color. This is necessary because some operations
    copy only the RGB values of a color, leaving the internal color
    field uninitialized.
*)
let realloc_color c =
  GDraw.color (`RGB((Gdk.Color.red c), (Gdk.Color.green c),(Gdk.Color.blue c)))
