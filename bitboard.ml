open Int64
open Command
open Color
type bitboard = (int64 * int64)

let leftup1 = shift_left one 63
let leftup2 = shift_left one 62
let leftup12 = logor leftup1 leftup2

(* constants for trans_pos *)
let upGuard =      (logor (of_int 0x3fffffffffffff00) leftup12)
let rightUpGuard = (logor (of_int 0x3f7f7f7f7f7f7f00) leftup2)
let right =        (logor (of_int 0x3f7f7f7f7f7f7f7f) leftup2)
let rightDown =            of_int 0x007f7f7f7f7f7f7f
let down =                 of_int 0x00ffffffffffffff
let leftDown =             of_int 0x00fefefefefefefe
let left =         (logor (of_int 0x3efefefefefefefe) leftup12)
let leftUp =       (logor (of_int 0x3efefefefefefe00) leftup12)

let corner_mask =  (logor (of_int 0x0100000000000081) leftup1)

(* (1,1)-(8,8) *)
let pos_bit i j = shift_right_logical (shift_right_logical leftup1 (8*(i-1))) (j-1)


let get_color (my,opp) pos mycolor color =
	if ((logand my pos) <> zero) then
		(if mycolor = color then mycolor else (opposite_color mycolor))
	else if ((logand opp pos) <> zero) then
		(if mycolor = color then (opposite_color mycolor) else mycolor)
	else
		none

let print_bitboard (my,opp) mycolor color =
  print_string (to_string my);print_string "\n";
  print_string (to_string opp);print_string "\n";
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for i=1 to 8 do
    print_int i; print_string "|";
    for j=1 to 8 do
      print_color (get_color (my,opp) (pos_bit j i) mycolor color); print_string " ";
    done;
    print_endline "";
  done;
  print_endline "  (X: Black,  O: White)"
 
let pop_count x =
	let cnt = ref 0 in
	for i=0 to 63 do
		 if (logand (shift_right_logical x i) one) = one then cnt := !cnt+1;
	done;
	!cnt

let get_myboard (my,opp) = my
let get_oppboard (my,opp) = opp

let count_corner b = pop_count (logand corner_mask b)

let count_empty (my,opp) =
	pop_count (lognot (logor my opp))

let swapturn board = 
	let (x,y) = board in (y,x)


(* int64 -> int -> int64 *)
let trans_pos pos dir =
	match dir with
	| 0 -> logand (shift_left pos 8) upGuard
	| 1 -> logand (shift_left pos 7) rightUpGuard
	| 2 -> logand (shift_right_logical pos 1) right
	| 3 -> logand (shift_right_logical pos 9) rightDown
	| 4 -> logand (shift_right_logical pos 8) down
	| 5 -> logand (shift_right_logical pos 7) leftDown
	| 6 -> logand (shift_left pos 1) left
	| 7 -> logand (shift_left pos 9) leftUp
	| _ -> failwith "invalid direction"
 
let rec convert_bitboard_sub acc mycolor board i j = 
	match (i,j) with
	| (9,8) -> acc
	| (9,j) -> convert_bitboard_sub acc mycolor board 1 (j+1)
	| (i,j) -> (
		let (x,y) = acc in
		(if board.(j).(i) = mycolor then
			(let nx = logor (shift_left x 1) one in
			 let ny = shift_left y 1 in
				 convert_bitboard_sub (nx,ny) mycolor board (i+1) j)
		 else if board.(j).(i) = (opposite_color mycolor) then
			(let nx = shift_left x 1 in
			 let ny = logor (shift_left y 1) one in
			 convert_bitboard_sub (nx,ny) mycolor board (i+1) j)
		 else
		 	(let nx = shift_left x 1 in
			 let ny = shift_left y 1 in
			 convert_bitboard_sub (nx,ny) mycolor board (i+1) j)))

let convert_bitboard mycolor board = convert_bitboard_sub (zero,zero) mycolor board 1 1


(* bitboard -> int64 *)
let valid_move_board board = 
	let (my,opp) = board in
	let horizontalWatchBoard = logand (logor (of_int 0x3e7e7e7e7e7e7e7e) leftup2) opp in
	let verticalWatchBoard = logand (of_int 0x00FFFFFFFFFFFF00) opp in
	let allSideWatchBoard = logand (of_int 0x007e7e7e7e7e7e00) opp in
	let blankboard = lognot (logor my opp) in
	let tmp = ref zero in
	let ret = ref zero in
	(* Left *)
	tmp := logand horizontalWatchBoard (shift_left my 1);
	tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
	ret := logand blankboard (shift_left !tmp 1);
	(* Right *)
	tmp := logand horizontalWatchBoard (shift_right_logical my 1);
	tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
	tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
	ret := logor !ret (logand blankboard (shift_right_logical !tmp 1));
	(* Up *)
	tmp := logand verticalWatchBoard (shift_left my 8);
	tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
	ret := logor !ret (logand blankboard (shift_left !tmp 8));
	(* Down *)
	tmp := logand verticalWatchBoard (shift_right_logical my 8);
	tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
	tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
	ret := logor !ret (logand blankboard (shift_right_logical !tmp 8));
	(* RightUp *)
	tmp := logand allSideWatchBoard (shift_left my 7);
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
	ret := logor !ret (logand blankboard (shift_left !tmp 7));
	(* LeftUp *)
	tmp := logand allSideWatchBoard (shift_left my 9);
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
	ret := logor !ret (logand blankboard (shift_left !tmp 9));
	(* RightDown *)
	tmp := logand allSideWatchBoard (shift_right_logical my 9);
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
	ret := logor !ret (logand blankboard (shift_right_logical !tmp 9));
	(* LeftDown *)
	tmp := logand allSideWatchBoard (shift_right_logical my 7);
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
	tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
	ret := logor !ret (logand blankboard (shift_right_logical !tmp 7));
	!ret

let valid_move_list b =
	let res = ref [] in
	for i = 1 to 8 do
		for j = 1 to 8 do
			if (logand (pos_bit i j) b) <> zero then res := (Mv (i,j))::(!res);
		done;
	done;
	(if !res = [] then res := [Pass]); 
	!res

let rec rev_board_dir acc (my,opp) mask dir = 
	if ((mask <> zero) && ((logand mask opp) <> zero)) then
		(rev_board_dir (logor acc mask) (my,opp) (trans_pos mask dir) dir)
	else
		(if ((logand mask my) <> zero) then
			acc
		 else
			zero)

let rec rev_board_dir_debug acc (my,opp) mask dir = 
	print_bitboard (acc,zero) 1 1;
	print_bitboard (mask,zero) 1 1;
	if ((mask <> zero) && ((logand mask opp) <> zero)) then
		(rev_board_dir_debug (logor acc mask) (my,opp) (trans_pos mask dir) dir)
	else
		(if ((logand mask my) <> zero) then
			acc
		 else
			zero)

let rev_board board pos =
	let res = ref zero in
	for i = 0 to 7 do
		res := logor !res (rev_board_dir zero board (trans_pos pos i) i);
	done;
	!res

let put_and_reverse board pos =
	let (my,opp) = board in
	let rev = rev_board board pos in 
	let nmy = logxor (logor pos rev) my in
	let nopp = logxor rev opp in
	(nmy,nopp)

let doMove_bitboard board com = 
	match com with
	| Mv (i,j) -> swapturn (put_and_reverse board (pos_bit i j))
	| Pass -> swapturn board
	| GiveUp -> swapturn board


		
