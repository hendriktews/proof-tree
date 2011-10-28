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
 * $Id: util.ml,v 1.15 2011/10/28 15:07:30 tews Exp $
 *)


(** Misc utility functions *)


(** {2 Missing from the List module} *)

(** Return the last element of a list and [assert false] on the empty
    list.
*)
let rec list_last = function
  | [] -> assert false
  | [a] -> a
  | _ :: rest -> list_last rest

(** List {!List.filter} but without the guarantee to preserve the
    order of the input list. Tail recursive.
*)
let rec list_filter_rev p accu = function
  | [] -> accu
  | x :: l -> 
    if p x then list_filter_rev p (x :: accu) l 
    else list_filter_rev p accu l


(** {2 Lists as Sets: Simple Set Operations} *)

(** [list_set_subset s1 s2] returns true precisely if [s1] is a
    (non-necessarily strict) subset of [s2].
*)
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


(** Return the union of [s1] and [s2] under the assumption that s1 and s2
    are disjoint. (So this is an ordinary union, NOT a disjoint union.)
*)
let list_set_union_disjoint s1 s2 = List.rev_append s1 s2


(** Add element [e] to set [s] under the assumption that [e] is not
    contained in [s]. Same as [list_set_union_disjoint [e] s].
*)
let list_set_add_nonpresent_element e s = e :: s


(** Internal tail-recursive worker function for
    {!list_set_remove_element}.
*)
let rec list_set_remove_element_rec e res = function
  | [] -> res
  | a :: s -> 
    if e = a 
    then List.rev_append res s
    else list_set_remove_element_rec e (a :: res) s


(** [list_set_remove_element e s] removes element [e] from set [s].
    Returns [s] without [e] (possibly differently ordered if [e] is
    not present in [s]. Only the first occurence of [e] is removed, so
    [s] should better be a proper set. Tail recursive.
*)
let list_set_remove_element e s =
  list_set_remove_element_rec e [] s


(** {2 Missing from the String module} *)

(** [search_char buf start stop c] searches for character [c] in the
    substring of [buf] starting at [start] and ending before [stop].
    Similar to {!String.index_from} but stop searching before [stop]
    and wrap the result in an [option] instead of raising an
    exception.
*)
let rec search_char buf start stop c =
  if start < stop then
    if buf.[start] = c 
    then Some start
    else search_char buf (start + 1) stop c
  else
    None

(** Remove all trailing newlines (['\n']) from the argument. *)
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


(** [string_starts buf start] returns [true] precisely if [start] is
    an initial prefix of [buf].
*)
let string_starts buf start =
  let buf_len = String.length buf in
  let start_len = String.length start in
  if buf_len >= start_len
  then String.sub buf 0 start_len = start
  else false


(** [string_ends buf tail] returns [true] if the last characters of [buf] 
    equal [tail].
*)
let string_ends buf tail =
  let buf_len = String.length buf in 
  let tail_len = String.length tail in
  buf_len >= tail_len &&
    (String.sub buf (buf_len - tail_len) tail_len) = tail


(** {2 Basic UTF-8 support} *)

(** [utf8_sequence_length s i] returns the byte-length of character at
    index [i] in [s]. May raise [Invalid_argument] if there is no
    valid UTF-8 at this position.
*)
let utf8_sequence_length s i =
  if int_of_char(s.[i]) land 0x80 = 0x00 then 1
  else if int_of_char(s.[i]) land 0xE0 = 0xC0 then 2
  else if int_of_char(s.[i]) land 0xF0 = 0xE0 then 3
  else if int_of_char(s.[i]) land 0xF8 = 0xF0 then 4
  else if int_of_char(s.[i]) land 0xFC = 0xF8 then 5
  else if int_of_char(s.[i]) land 0xFE = 0xFC then 6
  else raise (Invalid_argument "invalid UTF-8 sequence start byte")


(** Compute the number of characters in an UTF-8 string. May raise
    [Invalid_argument] if the argument is not valid UTF-8.
*)
let utf8_string_length s =
  let rec iter s len i res =
    if i >= len then res
    else iter s len (i + utf8_sequence_length s i) (res + 1)
  in
  iter s (String.length s) 0 0


(** [utf8_string_sub s len] returns the initial substring of the UTF-8
    string [s] with [len] characters. Raises 
    [Invalid_argument "utf8_string_sub"] if [len] is greater than the 
    number of characters in [s]. May raise [Invalid_argument] if the
    argument is not valid UTF-8.
*)
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


(** Count the number of lines in argument [s]. Returns at least 1. A
    final newline in [s] adds one to the result.
*)
let number_of_lines s =
  let lines = ref 1 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then incr lines
  done;
  !lines


(** Return the filehandle for debugmessages. Only used during
    debugging sessions. 
*)
let debugc =
  let copt = ref None in
  fun () -> match !copt with
    | Some c -> c
    | None ->
      let c = open_out "/tmp/prooftree-debug" in
      copt := Some c;
      c
