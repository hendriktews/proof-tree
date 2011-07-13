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
 * $Id: configuration.ml,v 1.9 2011/07/13 13:38:32 tews Exp $
 *)


(** Configuration record and global variables *)



type t = {
  turnstile_radius : int;
  turnstile_left_bar_x_offset : int;
  turnstile_left_bar_y_offset : int;
  turnstile_horiz_bar_x_offset : int;
  turnstile_line_width : int;
  turnstyle_number_x_offset : int;

  proof_command_length : int;

  subtree_sep : int;
  line_sep : int;

  level_distance : int;

  node_window_max_lines : int;

  button_1_drag_acceleration : float;
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

      turnstyle_number_x_offset = -(config.turnstile_line_width + 1);

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
    turnstyle_number_x_offset = 0;
    level_distance = 0;

    node_window_max_lines = 10;

    button_1_drag_acceleration = 4.0;
  }
  in
  update_sizes c radius


let current_config = ref default_configuration


let geometry_string = ref ""

let tee_input_file = ref (None : string option)

let debug = ref false
