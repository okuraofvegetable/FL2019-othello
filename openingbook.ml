open Hashtbl
open Bitboard
open Command
open Color

let opening_book : (bitboard, move) Hashtbl.t = Hashtbl.create 50000000
let initial_bitboard = ((Int64.of_int 0x810000000),(Int64.of_int 0x1008000000))

let com_of_int x = 
  let i = (x/8)+1 in
  let j = (x mod 8)+1 in
  Mv(i,j)

let rec read_file chan bb : unit =
  let a = input_char chan in
  if a = '\n' then
    read_file chan initial_bitboard
  else
    let num = (Char.code a)-33 in
    let com = (com_of_int num) in
      (*print_bitboard bb black black;
      print_string ((string_of_move com)^"\n");*)
      (Hashtbl.add opening_book bb com);
      read_file chan (doMove_bitboard bb com)

let make_opening_book u =
  let chan = open_in "./opening_book.gam" in
  (try read_file chan initial_bitboard with End_of_file ->
  close_in chan) 