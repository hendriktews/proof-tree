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
 * $Id: util.ml,v 1.8 2011/04/15 19:32:24 tews Exp $
 *)


(** Misc utility functions *)


let rec list_last = function
  | [] -> assert false
  | [a] -> a
  | _ :: rest -> list_last rest


let list_set_subset s1 s2 =
  List.for_all (fun e -> List.mem e s2) s1


(** Return the set-difference of s1 and s2. That is, return a list
    with all elements of [s1] that are not in [s2]. In the returned list 
    the elements are reversed with respect to the order of [s1].
*)
let list_set_diff_rev s1 s2 =
  List.fold_left
    (fun res e -> 
      if List.mem e s2 then res 
      else e :: res)
    [] s1


let rec search_char buf start stop c =
  if start < stop then
    if buf.[start] = c 
    then Some start
    else search_char buf (start + 1) stop c
  else
    None

let chop_final_newlines s =
  let i = ref (String.length s) in
  while !i > 0 && s.[!i - 1] = '\n' do
    decr i
  done;
  String.sub s 0 !i

(** Split string [s] at occurrences of [c]. Return the list of (non-zero)
    strings between sequences of [c].

    @param c split character
    @param s string to split
*)
let string_split c s =
  let len = String.length s in
  let rec iter i res =
    if i >= len 
    then List.rev res
    else
      let j =
	try String.index_from s i c 
	with Not_found -> len
      in
      iter (j + 1)
	(if i = j then res
	 else (String.sub s i (j - i)) :: res)
  in
  iter 0 []


let utf8_sequence_length s i =
  if int_of_char(s.[i]) land 0x80 = 0x00 then 1
  else if int_of_char(s.[i]) land 0xE0 = 0xC0 then 2
  else if int_of_char(s.[i]) land 0xF0 = 0xE0 then 3
  else if int_of_char(s.[i]) land 0xF8 = 0xF0 then 4
  else if int_of_char(s.[i]) land 0xFC = 0xF8 then 5
  else if int_of_char(s.[i]) land 0xFE = 0xFC then 6
  else raise (Invalid_argument "invalid UTF-8 sequence start byte")

let utf8_string_length s =
  let rec iter s len i res =
    if i >= len then res
    else iter s len (i + utf8_sequence_length s i) (res + 1)
  in
  iter s (String.length s) 0 0

let utf8_string_sub s len =
  let rec iter s i s_len res j res_len len hangover = 
    if len = 0 
    then begin
      assert(j = res_len);
      res
    end
    else if i < s_len
    then
      let s_i_len = utf8_sequence_length s i in
      if j + s_i_len <= res_len
      then begin
	res.[j] <- s.[i];
	for k = 1 to s_i_len - 1 do 
	  res.[j + k] <- s.[i + k]
	done;
	iter s (i + s_i_len) s_len 
	  res (j + s_i_len) res_len 
	  (len - 1) (hangover + s_i_len - 1)
      end
      else
	let n_res_len = res_len + hangover + s_i_len - 1 in
	let n_res = String.create n_res_len in
	String.blit res 0 n_res 0 j;
	iter s i s_len n_res j n_res_len len (1 - s_i_len)
    else
      raise (Invalid_argument "utf8_string_sub")
  in
  iter s 0 (String.length s) 
    (String.create len) 0 len 
    len 0
