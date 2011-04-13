(* 
 * prooftree --- proof tree display for Proof General
 * 
 * Copyright (C) 2011 Hendrik Tews
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License in file COPYING in this or one of the
 * parent directories for more details.
 * 
 * $Id: configuration.ml,v 1.6 2011/04/13 10:47:08 tews Exp $
 *)


(** Configuration record and global variables *)



type t = {
  turnstile_radius : int;
  turnstile_left_bar_x_offset : int;
  turnstile_left_bar_y_offset : int;
  turnstile_horiz_bar_x_offset : int;
  turnstile_line_width : int;

  proof_command_length : int;

  subtree_sep : int;
  line_sep : int;

  level_distance : int;
}


let update_sizes config radius =
  { config with 
      turnstile_radius = radius;
      turnstile_left_bar_x_offset = 
        int_of_float(-0.23 *. (float_of_int radius) +. 0.5);
      turnstile_left_bar_y_offset =
        int_of_float(0.65 *. (float_of_int radius) +. 0.5);
      turnstile_horiz_bar_x_offset =
        int_of_float(0.7 *. (float_of_int radius) +. 0.5);

      level_distance = 4 * radius
  }

let default_configuration = 
  let radius = 15 in
  let c = {
    turnstile_radius = radius;
    turnstile_line_width = 2;
    proof_command_length = 15;
    subtree_sep = 10;
    line_sep = 3;

    turnstile_left_bar_x_offset = 0;
    turnstile_left_bar_y_offset = 0;
    turnstile_horiz_bar_x_offset = 0;
    level_distance = 0;
  }
  in
  update_sizes c radius


let current_config = ref default_configuration


let geometry_string = ref ""

let tee_input_file = ref (None : string option)

let debug = ref false
