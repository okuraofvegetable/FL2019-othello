(**
  * Othello Server Script
  *)
open Command
open Play

module IM = Map.Make (struct
                        type t = int
                        let compare = compare
                      end)

type loglevel =
  | Silent
  | Normal
  | Verbose

let loglevel_to_string = function
  | Silent -> "Silent"
  | Normal -> "Normal"
  | Verbose -> "Verbose"

let opt_log_level = ref Normal
let opt_port = ref 3000
let opt_players_time = ref 600
let opt_timeout = ref 60000
let opt_byoyomi = ref 500
let opt_players_number = ref 2
let opt_rounds = ref 4
let opt_concurrency = ref 1

(* 文字列のpadding *)
let pad str len =
  let l = String.length str in
    if l >= len then
      str
    else
      let s = String.make (len-l) ' ' in
        str ^ s

(* 現在の時間の文字列 *)
let time_str () =
  let tm = Unix.localtime @@ Unix.gettimeofday () in
    Printf.sprintf "\x1b[1;35m[%04d-%02d-%02d %02d:%02d:%02d JST]\x1b[0m"
      (1900 + tm.Unix.tm_year)
      (1 + tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec


(* Parse Args *)
let help_func = ref (fun () -> ())
let help_func2 () =
  (!help_func)();
  exit 0

let options = [
  ("--verbose", Arg.Unit (fun () -> opt_log_level := Verbose), " verbose");
  ("-p", Arg.Set_int opt_port, " port number (default = 3000)");
  ("-t", Arg.Set_int opt_players_time, " time assigned to each player (in second; default = 600)");
  ("--timeout", Arg.Set_int opt_timeout, " time out, i.e. how long the server waits for a command (in millisecond; default = 60000)");
  ("-T", Arg.Set_int opt_byoyomi, " byoyomi time (in millisecond; default = 500)");
  ("-n", Arg.Set_int opt_players_number, " number of players (default = 2)");
  ("-r", Arg.Set_int opt_rounds, " number of rounds (multiple of 2; default = 4)");
  ("-c", Arg.Set_int opt_concurrency, " number of maximum concurrent matches (default = 1)");
]
let hidden = [
  ("--port", Arg.Set_int opt_port, " port number (default = 3000)");
  ("--playerstime", Arg.Set_int opt_players_time, " time assigned to each player (in second; default = 600)");
  ("--byoyomi", Arg.Set_int opt_byoyomi, " byoyomi time (in millisecond; default = 500)");
  ("--players", Arg.Set_int opt_players_number, " number of players (default = 2)");
  ("--rounds", Arg.Set_int opt_rounds, " number of rounds (multiple of 2; default = 4)");
  ("--concurrency", Arg.Set_int opt_concurrency, " number of maximum concurrent matches (default = 1)");
  ("-h", Arg.Unit help_func2, "");
  ("--help", Arg.Unit help_func2, "");
]

let usage_msg = String.concat "\n" [
  "Usage:";
  "  reversi-serv -p PORT ...";
  "";
];;
help_func := fun () -> Arg.usage (Arg.align options) usage_msg;;

(* Log level *)
let verbose f =
  if !opt_log_level = Verbose then
    f ()
  else
    ()

(* Wait for clients *)
type client = Client of Unix.file_descr * in_channel * out_channel * string ref * score ref
and score = {
  win: int;
  lose: int;
}
let close_client (Client (_, inchan, _, _, _)) =
  close_in inchan;
  ()
let send_client (Client (_, _, outchan, name, _)) =
  Printf.ksprintf
    (fun str ->
       verbose (fun () -> Format.eprintf "\x1b[36mSending to %s: %s\x1b[0m@." !name str);
       output_string outchan str;
       flush outchan)
let name_of_client (Client (_, _, _, nr, _)) = !nr
(* file descriptorをidとして使用 *)
let id_of_client (Client (fd, _, _, _, _)) = fd
let add_game_client (Client (_, _, _, _, score)) = function
  | "WIN" ->
      score := {
        win = !score.win + 1;
        lose = !score.lose;
      }
  | "LOSE" ->
      score := {
        win = !score.win;
        lose = !score.lose + 1;
      }
  | _ -> ()

exception SessionEnd

let put_waiting_msg = function
  | 0 -> ()
  | 1 ->
      Printf.eprintf "\x1b[34mWaiting 1 connection ...\x1b[0m\n";
      flush stderr
  | n ->
      Printf.eprintf "\x1b[34mWaiting %d connetions ...\x1b[0m\n" n;
      flush stderr

(* n人の総当り対戦カードを作る (ただし同じ組み合わせで逆のは作らない)*)
let make_cards n =
  let rec aux a = function
    | 0 -> a
    | n -> aux ((n-1)::a) (n-1) in
  List.concat
    (List.map
       (fun y ->
          List.filter
            (fun (x, y) -> x < y) @@
          List.map
            (fun x -> (x, y)) @@
          aux [] n)
       (aux [] n))

exception PlayInvalidCommand of client
exception PlayInvalidMove of client
exception PlayGiveUp of client
exception PlayTimeUp of client

(* session開始 *)
let rec start_session clients =
  (* Sessionは子プロセスで行う *)
  let p1 = Unix.fork () in
  if p1 = 0 then
    begin
      (* 子 *)
      let p2 = Unix.fork () in
      if p2 = 0 then begin
        (* 孫 *)
          session clients;
          exit 0
        end
      else
        exit 0
    end
  else begin
    (* 親はもうclientsのことは知らない *)
    List.iter close_client clients;
    ignore (Unix.waitpid [] p1)
  end
and session clients =
  (* XXX 既に消えてる人のことを考えてないけどまあいいや *)
  (* 各クライアントからopenを受け取る *)
  Format.eprintf "%s Session started (%d players)\n@."
    (time_str ())
    (List.length clients);
  try
    begin
      (* 名前を聞く *)
      List.iter
        (fun (Client (_, inchan, outchan, nr, _)) ->
           let c = parse_command_from inchan in
             match c with
               | Open name ->
                   verbose (fun () -> Format.eprintf "Registered %s as %s@." !nr name);
                   nr := name;
               | _ ->
                   (* は???????????????????? *)
                   verbose (fun () -> Format.eprintf "\x1b[33mReceived invalid command from %s\x1b[0m@." !nr);
                   byebye clients;
                   raise SessionEnd)
        clients;
      (* 総当り対戦 *)
      let cards = make_cards !opt_players_number in
      let cards2 =
        List.map
          (fun (x, y) -> (y, x))
          cards in
        play_all 0 cards cards2 clients;
        byebye clients;
        raise SessionEnd
    end
  with SessionEnd ->
  Format.eprintf "%s Session ended@."
    (time_str ());
  Format.eprintf "name,score,win,lose@.";
    (* statを表示 *)
  let longest =
    List.fold_left
      max
      0 @@
    List.map
      (fun c -> String.length (name_of_client c))
      clients in
    List.iter
      (fun (Client (_, _, _, name, score)) ->
         Format.eprintf "\x1b[36m%s, %d, %d, %d\x1b[0m@."
           (pad !name longest)
           (!score.win - !score.lose)
           !score.win
           !score.lose)
      clients;
    List.iter close_client clients

and play_all round cards cards2 clients =
  if round >= !opt_rounds then
    ()
  else begin
    (* cardsで示された対戦を行う *)
    let game_max = List.length cards in
    (* 今何番目のゲームか数える *)
    let game_num = ref 1 in
    (* 対戦中のクライアントを記録 *)
    let playing = Array.make (List.length clients) false in
    (* 対戦の数を記録 *)
    let conc_count = ref 0 in
    (* pidと対戦カードのメモ *)
    let env = ref IM.empty in
    let wait_one_process () =
      let (pid, _) = Unix.waitpid [] 0 in
      (* 空いたかも *)
      let ((b, w), piperead) = IM.find pid !env in
        playing.(b) <- false;
        playing.(w) <- false;
        conc_count := !conc_count - 1;
      (* pipeから読む *)
      let chan = Unix.in_channel_of_descr piperead in
      let blackresult = input_line chan in
        add_game_client (List.nth clients b) blackresult;
      let whiteresult = input_line chan in
        add_game_client (List.nth clients w) whiteresult;
        close_in chan;
    in
    let rec start_one_play cards =
      match cards with
        | [] ->
            (* もうおわり *)
            None
        | _ when !conc_count >= !opt_concurrency ->
            (* 並列性がいっぱいなので次にいけない *)
            None
        | _ ->
            (* 開始できそうなカードをはじめから探す *)
            let rec search_card acc = function
              | [] -> None
              | (b, w) :: cards' ->
                  if (not playing.(b)) && (not playing.(w)) then
                    Some ((b, w), List.rev_append acc cards')
                  else
                    search_card ((b, w)::acc) cards'
            in
              match search_card [] cards with
                | Some ((b, w), cards') ->
                    (* カードがあった *)
                    let bc = List.nth clients b in
                    let wc = List.nth clients w in
                      playing.(b) <- true;
                      playing.(w) <- true;
                    (* 通信用パイプ *)
                    let (piperead, pipewrite) = Unix.pipe () in
                      let pid = Unix.fork () in
                      conc_count := !conc_count + 1;
                        if pid = 0 then begin
                          (* 子 *)
                          Unix.close piperead;
                          play round !opt_rounds !game_num game_max bc wc pipewrite;
                          exit 0;
                        end else begin
                          (* 親 *)
                          env := IM.add pid ((b, w), piperead) !env;
                          Unix.close pipewrite;
                        end;
                        Some (cards', pid)
                | None ->
                    (* 今は対戦できるカードがない *)
                    None
    in
    (* ゲームを全部終えるまでループする *)
    let rec loop cards =
      match cards with
        | [] -> ()
        | _ -> begin
            match start_one_play cards with
              | None ->
                  (* 今は開始できない *)
                  wait_one_process ();
                  loop cards
              | Some(cards', _) ->
                  game_num := !game_num + 1;
                  loop cards'
          end
    in
      loop cards;
    (* 全プロセスを待つ *)
    let rec wait_games () =
      if !conc_count <= 0 then
        ()
      else begin
        wait_one_process ();
        wait_games ();
      end
    in
      wait_games ();

      (* 次のラウンドへ *)
      play_all (round+1) cards2 cards clients
  end

and play round round_max gamenum game_max bc wc pipewrite =
  Format.eprintf "%s Round %d/%d, Game %d/%d (%s vs %s)@."
    (time_str ())
    (round+1)
    round_max
    gamenum
    game_max
    (name_of_client bc)
    (name_of_client wc);
  (* Boarsを準備 *)
  let board = ref (init_board()) in
  (* START stateにする *)
  let time_bc = ref (!opt_players_time * 1000) in
  let time_wc = ref (!opt_players_time * 1000) in
  send_client bc "START BLACK %s %d\n" (name_of_client wc) !time_bc;
  send_client wc "START WHITE %s %d\n" (name_of_client bc) !time_wc;
  (* ひとつの手を受信する *)
  let wait_for_move (Client (_, inchan, _, _, _) as cl) time =
    let start_time = Unix.gettimeofday () in
    let com = parse_command_from inchan in
    let end_time = Unix.gettimeofday () in
    let diff_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
      time := !time - diff_ms;
      match com with
        | Move m -> m
        | _ ->
            (* invalid command *)
            raise (PlayInvalidCommand cl)
  in
  (* 前回パスしたかどうかのフラグ *)
  let pass_flg = ref false in
  (* プレイを行う *)
  let rec plays ((cl_curr, time_curr, color_curr) as curr) ((cl_oppo, time_oppo, color_oppo) as oppo) =
    let m = wait_for_move cl_curr time_curr in
      match m with
        | Mv (x, y) ->
            pass_flg := false;
            if is_valid_move !board color_curr (x, y) then
              (* 許す *)
              begin
                board := doMove !board m color_curr;
                send_client cl_curr "ACK %d\n" !time_curr;
                (* 時間が切れていないか? *)
                if !time_curr < ((-1) * !opt_byoyomi) then
                  raise (PlayTimeUp cl_curr);
                (* 秒読みモード *)
                if !time_curr <= 0 then
                  time_curr := !opt_byoyomi;
                send_client cl_oppo "MOVE %s\n" (string_of_move m);
                plays oppo curr
              end
            else
             (* 許さない!!!!!!!!!!!111111!1!!!111!11111 *)
              raise (PlayInvalidMove cl_curr)
        | Pass ->
            if valid_moves !board color_curr = [] then
              (* 許す *)
              if !pass_flg then
                (* 両者Passした *)
                normal_end_game ()
              else begin
                pass_flg := true;
                send_client cl_curr "ACK %d\n" !time_curr;
                send_client cl_oppo "MOVE %s\n" (string_of_move m);
                plays oppo curr
              end
            else
              (* 許さない!!!!!!!!!!!111111!1!!!111!11111 *)
              raise (PlayInvalidMove cl_curr)
        | GiveUp ->
            raise (PlayGiveUp cl_curr)
  and normal_end_game () =
    (* 終了処理 *)
    let black_count = count !board Color.black in
    let white_count = count !board Color.white in
    let black_result =
      if black_count > white_count then
        "WIN"
      else if black_count < white_count then
        "LOSE"
      else
        "TIE" in
    let white_result =
      if black_count < white_count then
        "WIN"
      else if black_count > white_count then
        "LOSE"
      else
        "TIE" in
      end_game black_result white_result "DOUBLE_PASS";
  and end_game black_result white_result reason =
    let black_count = count !board Color.black in
    let white_count = count !board Color.white in
    (* 親プロセスに結果を報告 *)
    let chan = Unix.out_channel_of_descr pipewrite in
    Printf.fprintf chan "%s\n" black_result;
    Printf.fprintf chan "%s\n" white_result;
    flush chan;
    close_out chan;
    add_game_client bc black_result;
    add_game_client wc white_result;
    (* 結果を教える *)
    send_client bc "END %s %d %d %s\n" black_result black_count white_count reason;
    send_client wc "END %s %d %d %s\n" white_result white_count black_count reason;
    Format.eprintf "%s \x1b[32mRound %d/%d, Game %d/%d Result: %s %s (%d), %s %s (%d); (%d-%d, %s)\x1b[0m@."
      (time_str ())
      (round+1)
      round_max
      gamenum
      game_max
      (name_of_client bc)
      black_result
      (!time_bc)
      (name_of_client wc)
      white_result
      (!time_wc)
      black_count
      white_count
      reason;
  in
    try
      plays (bc, time_bc, Color.black) (wc, time_wc, Color.white)
    with
      | PlayInvalidCommand cl ->
          let black_result =
            if id_of_client cl = id_of_client bc then
              "LOSE"
            else
              "WIN" in
          let white_result =
            if id_of_client cl = id_of_client wc then
              "LOSE"
            else
              "WIN" in
          end_game black_result white_result "INVALID_COMMAND";
      | PlayInvalidMove cl ->
          let black_result =
            if id_of_client cl = id_of_client bc then
              "LOSE"
            else
              "WIN" in
          let white_result =
            if id_of_client cl = id_of_client wc then
              "LOSE"
            else
              "WIN" in
          end_game black_result white_result "INVALID_MOVE";
      | PlayGiveUp cl ->
          let black_result =
            if id_of_client cl = id_of_client bc then
              "LOSE"
            else
              "WIN" in
          let white_result =
            if id_of_client cl = id_of_client wc then
              "LOSE"
            else
              "WIN" in
          end_game black_result white_result "GIVEUP";


and byebye clients =
  (* 全てのclientにBYEを送る *)
  let stat = Buffer.create 0 in
    List.iter
      (fun (Client (_, _, _, name, score)) ->
         Buffer.add_string stat (Printf.sprintf " %s %d %d %d" !name (!score.win - !score.lose) !score.win !score.lose))
      clients;
  let stat = Buffer.contents stat in
  List.iter
    (fun c ->
       send_client c "BYE%s\n" stat)
    clients;

and parse_command_from inchan =
  try
    let line = input_line inchan in
    let lexbuf = Lexing.from_string line in
      CommandParser.comm CommandLexer.token lexbuf
  with
    | End_of_file -> Empty


let wait_for_clients () =
  let addr = Unix.ADDR_INET(Unix.inet_addr_any, !opt_port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock addr;
    Unix.listen sock (!opt_players_number * 2);
  (* 残りn人待つ *)
  let rec w clients n =
    if n <= 0 then begin
      start_session (List.rev clients);
      w [] !opt_players_number
    end else
      begin
        put_waiting_msg n;
        let (sock, _) = Unix.accept sock in
        Unix.setsockopt_float sock Unix.SO_RCVTIMEO ((float_of_int (!opt_timeout)) /. 1000.0);
        let inchan = Unix.in_channel_of_descr sock in
        let outchan = Unix.out_channel_of_descr sock in
        let kari_client_name = Printf.sprintf "Client #%d" (!opt_players_number - n + 1) in
        let c = Client (sock, inchan, outchan, ref kari_client_name, ref {
          win = 0;
          lose = 0;
        }) in
        let clients' = c::clients in
          w clients' (n-1)
      end in
  w [] !opt_players_number

(* main *)
let _ =
  Arg.parse (options@hidden) (fun _ -> ()) usage_msg;
  Printf.eprintf "Server Condifuration:\n";
  Printf.eprintf "\tListening:           %d\n" !opt_port;
  Printf.eprintf "\tTime out:            %d\n" !opt_timeout;
  Printf.eprintf "\tEach Player's Time:  %d\n" !opt_players_time;
  Printf.eprintf "\tByoyomi:             %d\n" !opt_byoyomi;
  Printf.eprintf "\t#Players:            %d\n" !opt_players_number;
  Printf.eprintf "\t#Rounds:             %d\n" !opt_rounds;
  Printf.eprintf "\tVerbosity:           %s\n" (loglevel_to_string !opt_log_level);
  Printf.eprintf "\tDig. of Concurrency: %d\n" !opt_concurrency;
  wait_for_clients ()
