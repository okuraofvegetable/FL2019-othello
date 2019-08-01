# FL2019-othello
Othello AI implemented in assignment of FL class

# How to use
 
```sh
$ make
$ ./reversi-serv -p 3000 -t 60
$ ./reversi -p 3000 -n YOUR_NAME
```
# What I implemented
 - Nega-alpha
 - Move ordering by minimizing number of valid moves of enemy
 - Bitboard (using Int64 module)
 - Transposition table (using Hashtbl module)
 - Opening Book

# Strategy

## Early stage
use data in [openingbook of logistello](https://skatgame.net/mburo/log.html)
## Middle stage
search 9 turns and select best move by evaluation function
## Final stage
search 18 turns. (also searching limited to 5000000 nodes in last 19~22 turns)
