open Array
open Color
open Command
open Bitboard
open Openingbook


exception AssertError of string 

type board = color array array

let init_board () =
  let board = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let count board color =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s

let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (board.(i).(j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"

let print_move move =
  match move with
  | GiveUp -> print_string "GiveUp\n"
  | Pass -> print_string "Pass\n"
  | Mv (i,j) -> print_string ("Mv "^(string_of_int i)^","^(string_of_int j)^"\n")

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then
      r
    else
      [] in
    f (di,dj) (i,j) []



let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with
      [] -> false
    | _  -> true

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j)


let doMove board com color =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in
	let _  = board.(i).(j) <- color in
	  board

let copy_board board = 
  let newboard = Array.make_matrix 10 10 none in
  for j=0 to 9 do
    for i=0 to 9 do
      newboard.(i).(j) <- board.(i).(j);
    done;
  done;
  newboard

let copy_and_doMove board com color = 
  let newboard = copy_board board in
  doMove newboard com color

let rotate = ref 0

let copy_and_rotate_board board = 
  let newboard = copy_board board in
  (if !rotate = 1 then
    (for i=1 to 8 do
      for j=1 to 8 do
        newboard.(j).(i) <- board.(i).(j);
      done;
    done;())
  else if !rotate = 2 then
    (for i=1 to 8 do
      for j=1 to 8 do
        newboard.(9-j).(9-i) <- board.(i).(j);
      done;
    done;())
  else if !rotate = 3 then 
    (for i=1 to 8 do
      for j=1 to 8 do
        newboard.(9-i).(9-j) <- board.(i).(j);
      done;
    done;())
  else ());
  newboard

let rev_rotate_command com = 
  match com with
  | Mv (i,j) -> 
    (if !rotate = 1 then Mv(j,i)
     else if !rotate = 2 then Mv((9-j),(9-i))
     else if !rotate = 3 then Mv((9-i),(9-j))
     else Mv(i,j)) 
  | c -> c 

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)


let valid_moves board color =
  let ls = [1;2;3;4;5;6;7;8] in
  List.filter (is_valid_move board color)
    (mix ls ls)

let valid_coms board color = 
  let move_list = valid_moves board color in
  let com_list = List.map (fun (i,j) -> (Mv (i,j))) move_list in
  match com_list with
  | [] -> [Pass]
  | _ -> com_list    



type search_mode = Last | Middle

(*

let old_count_corner board color =
  let cnt = ref 0 in
    if board.(1).(1) = color then cnt := !cnt +1;
    if board.(1).(8) = color then cnt := !cnt +1;
    if board.(8).(1) = color then cnt := !cnt +1;
    if board.(8).(8) = color then cnt := !cnt +1;
    !cnt

let old_count_edge board color = 
  let cnt = ref 0 in
    for i=2 to 7 do
      if board.(i).(1) = color then cnt := !cnt +1;
      if board.(i).(8) = color then cnt := !cnt +1;
      if board.(1).(i) = color then cnt := !cnt +1;
      if board.(8).(i) = color then cnt := !cnt +1;
    done;
    !cnt

let f board color = 
  (old_count_corner board color)*10+(List.length (valid_moves board color))

*)

let last_table : (bitboard,(int * move)) Hashtbl.t = Hashtbl.create 10000000


let eval_bitboard_middle (now,opp) = 
  (((count_corner now)*7)+(pop_count (valid_move_board (now,opp))))
    -(((count_corner opp)*7)+(pop_count (valid_move_board (opp,now))))

let eval_bitboard_last (now,opp) = 
  ((pop_count now)-(pop_count opp))


(*
let eval_board mycolor color board mode = 
  match mode with
  | Last -> 
    (let value = ((count board mycolor) - (count board (opposite_color mycolor))) in
    if (color = mycolor) then value else (-value))
  | Middle ->
    (let value = (f board mycolor) - (f board (opposite_color mycolor)) in
    if (color = mycolor) then value else (-value))  

let old_next_valid_move_size board com color =
  let next = copy_and_doMove board com color in
  let opp = opposite_color color in
  List.length (valid_moves next opp) 
*)

let next_valid_move_size board com =
  let next = doMove_bitboard board com in
  pop_count (valid_move_board next) 


let rec print_com_list com_list =
  match com_list with
  | [] -> print_string "\n"
  | com::rest -> print_string (string_of_move com);print_string "; ";print_com_list rest

let counter = ref 0
exception Timeout

let max a b =
	if a > b then a else b

let rec negamax_rec_last (value,mv) com_list depth board mycolor color alpha beta =
  match com_list with 
  | [] -> (value,mv)
  | (com::rest) -> 
    let next = doMove_bitboard board com in
    let (v,_) = get_optimal_negamax_last (depth-1) next mycolor (opposite_color color) (-beta) (-(max value alpha)) in  
    let (nval,nmove) = if (-v)>value then (-v,com) else (value,mv) in
    if ((nval>0) && (mycolor = color)) then
      (nval,nmove) 
    else (
      if nval>=beta then (nval,nmove) else (negamax_rec_last (nval,nmove) rest depth board mycolor color alpha beta)
    )
and negamax_rec_last2 (value,mv) com_list depth board mycolor color alpha beta =
  match com_list with 
  | [] -> (value,mv)
  | (com::rest) -> 
    let next = doMove_bitboard board com in
    let (v,_) = get_optimal_negamax_last2 (depth-1) next mycolor (opposite_color color) (-beta) (-(max value alpha)) in  
    let (nval,nmove) = if (-v)>value then (-v,com) else (value,mv) in
    if ((nval>0) && (mycolor = color)) then
      (nval,nmove) 
    else (
      if nval>=beta then (nval,nmove) else (negamax_rec_last2 (nval,nmove) rest depth board mycolor color alpha beta)
    )
(* eval value * move *)
and get_optimal_negamax_last2 depth board mycolor color alpha beta =
  (*print_string "debug-------------------\n";
  print_board board;*)
  counter := !counter+1;
  if (!counter > 5000000) then (raise Timeout)
  else
  (
    let res = Hashtbl.find_opt last_table board in
    match res with
    | Some c -> c
    | None ->
    (
      if ((depth = 0) || ((count_empty board) = 0)) then
      (
        let (v,m) = ((eval_bitboard_last board),Pass) in
        (*
        (if mode = Last then 
          (
           print_string ("mycolor : "^(string_of_color mycolor)^"\n");
           print_string ("turn: "^(string_of_color color)^"\n");
           print_string "value :";
           print_int v;
           print_string "\n";
           print_bitboard board mycolor color
        ));
        *)
        (*(if !counter <= 1000000 then 
          Hashtbl.add last_table board (v,m)
        else ());*)Hashtbl.add last_table board (v,m);(v,m)
      )
      else
      ( 
        let com_list = valid_move_list (valid_move_board board) in
        let com_list_sorted = 
          List.sort
          (fun s t -> ((next_valid_move_size board s) - (next_valid_move_size board t)))
          com_list in
        let (v,m) = (negamax_rec_last2 (-1000,Pass) com_list_sorted depth board mycolor color alpha beta) in
        (*
        (if mode = Last then
          (
           print_string ("turn: "^(string_of_color color)^"\n"); 
           print_bitboard board mycolor color;
           print_string "value :";
           print_int v;
           print_string "\n")); *)
        
        (*(if !counter <= 1000000 then
          Hashtbl.add last_table board (v,m)
        else ());*) Hashtbl.add last_table board (v,m);(v,m)
      )
    )
  )
and get_optimal_negamax_last depth board mycolor color alpha beta =
  (*print_string "debug-------------------\n";
  print_board board;*)
  counter := !counter+1;
  (
    let res = Hashtbl.find_opt last_table board in
    match res with
    | Some c -> c
    | None ->
    (
      if ((depth = 0) || ((count_empty board) = 0)) then
      (
        let (v,m) = ((eval_bitboard_last board),Pass) in
        (*
        (if mode = Last then 
          (
           print_string ("mycolor : "^(string_of_color mycolor)^"\n");
           print_string ("turn: "^(string_of_color color)^"\n");
           print_string "value :";
           print_int v;
           print_string "\n";
           print_bitboard board mycolor color
        ));
        *)
        (*(if !counter <= 1000000 then 
          Hashtbl.add last_table board (v,m)
        else ());*)Hashtbl.add last_table board (v,m);(v,m)
      )
      else
      ( 
        let com_list = valid_move_list (valid_move_board board) in
        let com_list_sorted = 
          List.sort
          (fun s t -> ((next_valid_move_size board s) - (next_valid_move_size board t)))
          com_list in
        let (v,m) = (negamax_rec_last (-1000,Pass) com_list_sorted depth board mycolor color alpha beta) in
        (*
        (if mode = Last then
          (
           print_string ("turn: "^(string_of_color color)^"\n"); 
           print_bitboard board mycolor color;
           print_string "value :";
           print_int v;
           print_string "\n")); *)
        
        (*(if !counter <= 1000000 then
          Hashtbl.add last_table board (v,m)
        else ());*) Hashtbl.add last_table board (v,m);(v,m)
      )
    )
  )

let rec negamax_rec_middle (value,mv) com_list depth board mycolor color alpha beta =
  match com_list with 
  | [] -> (value,mv)
  | (com::rest) -> 
    let next = doMove_bitboard board com in
    let (v,_) = get_optimal_negamax_middle (depth-1) next mycolor (opposite_color color) (-beta) (-(max value alpha)) in  
    let (nval,nmove) = if (-v)>value then (-v,com) else (value,mv) in
    if nval>=beta then (nval,nmove) else (negamax_rec_middle (nval,nmove) rest depth board mycolor color alpha beta)
(* eval value * move *)
and get_optimal_negamax_middle depth board mycolor color alpha beta =
  (*print_string "debug-------------------\n";
  print_board board;*)
  counter := !counter+1;
  if ((depth = 0) || ((count_empty board) = 0)) then
    let (v,m) = ((eval_bitboard_middle board),Pass) in
    (*
    (if mode = Last then 
      (
       print_string ("mycolor : "^(string_of_color mycolor)^"\n");
       print_string ("turn: "^(string_of_color color)^"\n");
       print_string "value :";
       print_int v;
       print_string "\n";
       print_bitboard board mycolor color
    ));
    *)
    (v,m)
  else 
    let com_list = valid_move_list (valid_move_board board) in
    let com_list_sorted = 
      List.sort
      (fun s t -> ((next_valid_move_size board s) - (next_valid_move_size board t)))
      com_list in
    let (v,m) = (negamax_rec_middle (-1000,Pass) com_list_sorted depth board mycolor color alpha beta) in
    (*
    (if mode = Last then
      (
       print_string ("turn: "^(string_of_color color)^"\n"); 
       print_bitboard board mycolor color;
       print_string "value :";
       print_int v;
       print_string "\n")); *)
    
    (v,m)


let play board color =
  (*print_board (copy_and_rotate_board board);
  rotate := 1;
  print_board (copy_and_rotate_board board);
  rotate := 2;
  print_board (copy_and_rotate_board board);
  rotate := 3;
  print_board (copy_and_rotate_board board);
  rotate := 0;*)
  print_board board;
  print_string "thinking....\nnumber of nodes in privious search : ";
  print_int !counter;
  print_string "\n";
  counter := 0;
  let empty = count board none in
  if empty = 60 then
    (Hashtbl.clear last_table;rotate := 0;Mv (4,3))
  else if empty = 59 then
    ((if board.(4).(3) <> none
      then (rotate := 0)
    else if board.(3).(4) <> none
      then (rotate := 1)
    else if board.(6).(5) <> none
      then (rotate := 2)
    else (rotate := 3));
    Hashtbl.clear last_table;
    let bb = (convert_bitboard color (copy_and_rotate_board board)) in
    let com_book = Hashtbl.find_all opening_book bb in
    (if com_book = [] then
        (let (_,com) = get_optimal_negamax_middle 9 bb color color (-1000) 1000 in
        print_string (("\n"^(string_of_move com))^"\n");
        rev_rotate_command com)
      else
        let k = Random.int (List.length com_book) in
        rev_rotate_command (List.nth com_book k)))
  else
  (*print_bitboard bb color color;
  print_bitboard ((valid_move_board bb),Int64.zero) color color;*)
  let ms = valid_moves board color in
  let bb = (convert_bitboard color (copy_and_rotate_board board)) in
  (
    if ms = [] then
      Pass 
    else if (count board none) <= 18 then
      let (v,com) = get_optimal_negamax_last 36 bb color color (-1000) 1000 in
      print_string "value : ";
      print_int v;
      print_string (("\n"^(string_of_move com))^"\n");
      rev_rotate_command com
    else if (count board none) <= 22 then
      try (
        let (v,com) = get_optimal_negamax_last2 44 bb color color (-1000) 1000 in
        print_string "win! value : ";
        print_int v;
        print_string (("\n"^(string_of_move com))^"\n");
        rev_rotate_command com
      ) with
      | Timeout -> 
        (
        print_string "fail!\nnumber of nodes : ";
        print_int !counter;
        print_string "\n";
        counter := 0;
        let (_,com) = get_optimal_negamax_middle 9 bb color color (-1000) 1000 in
        print_string (("\n"^(string_of_move com))^"\n");
        rev_rotate_command com)
    else if (count board none) <= 64 then
      let com_book = Hashtbl.find_all opening_book bb in
      (if com_book = [] then
        (let (_,com) = get_optimal_negamax_middle 9 bb color color (-1000) 1000 in
        print_string (("\n"^(string_of_move com))^"\n");
        rev_rotate_command com)
      else
        let k = Random.int (List.length com_book) in
        rev_rotate_command (List.nth com_book k))
    else
        (
          let k = Random.int (List.length ms) in
          let (i,j) = List.nth ms k in
          Mv (i,j)
        )
  )
  







let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board
