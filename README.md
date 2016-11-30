# TicTacToe-Bot

## Bot which makes logical moves in a simple Tic Tac Toe game board

Works only with data encoded as JSON without dictonary and passed in string form.
Ex.: "[[\"x\",  0,  \"y\",   0,  \"v\", \"x\"],  [\"x\",  2, \"y\",  2,  \"v\",  \"o\"]]"
### Usage

Set up module with cabal:
```
cabal sandbox init
cabal install -j
```
Launch main function with parameters: 1st is for game room, 2nd for player number
```
.cabal-sandbox/bin/TicToc room number
```
