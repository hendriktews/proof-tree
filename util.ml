
let rec list_last = function
  | [] -> assert false
  | [a] -> a
  | _ :: rest -> list_last rest


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
