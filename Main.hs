{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
  -- play 5

    let body = L8.pack $ makeMove ""
    let request
         = setRequestMethod "POST"
         $ setRequestPath "/game/abcd/player/1"
         $ setRequestHeaders [("Content-Type","application/json+list")]
         $ setRequestBodyLBS body
         $ request'
    response <- httpLBS request
    -- print response

    -- let gameName = "nespesiuAntDeadline"
    -- print gameName
    -- let path = "/game/" ++ gameName ++ "/player/1"
    ----------------------------------------------------------------------------------------
    request' <- parseRequest "http://tictactoe.homedir.eu"
    let request
         = setRequestMethod "GET"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Accept","application/json+list")]
         $ request'
    response <- httpLBS request
    print response

    request' <- parseRequest "http://tictactoe.homedir.eu"

    let body = L8.pack $ makeMove $ L8.unpack $ getResponseBody response
    let request
         = setRequestMethod "POST"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Content-Type","application/json+list")]
         $ setRequestBodyLBS body
         $ request'
    response <- httpLBS request
    print response
    ----------------------------------------------------------------------------------------
    request' <- parseRequest "http://tictactoe.homedir.eu"
    let request
         = setRequestMethod "GET"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Accept","application/json+list")]
         $ request'
    response <- httpLBS request
    print response

    request' <- parseRequest "http://tictactoe.homedir.eu"

    let body = L8.pack $ makeMove $ L8.unpack $ getResponseBody response
    let request
         = setRequestMethod "POST"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Content-Type","application/json+list")]
         $ setRequestBodyLBS body
         $ request'
    response <- httpLBS request
    print response
    ----------------------------------------------------------------------------------------
    request' <- parseRequest "http://tictactoe.homedir.eu"
    let request
         = setRequestMethod "GET"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Accept","application/json+list")]
         $ request'
    response <- httpLBS request
    print response

    request' <- parseRequest "http://tictactoe.homedir.eu"

    let body = L8.pack $ makeMove $ L8.unpack $ getResponseBody response
    let request
         = setRequestMethod "POST"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Content-Type","application/json+list")]
         $ setRequestBodyLBS body
         $ request'
    response <- httpLBS request
    print response
    ----------------------------------------------------------------------------------------
    request' <- parseRequest "http://tictactoe.homedir.eu"
    let request
         = setRequestMethod "GET"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Accept","application/json+list")]
         $ request'
    response <- httpLBS request
    print response

    request' <- parseRequest "http://tictactoe.homedir.eu"

    let body = L8.pack $ makeMove $ L8.unpack $ getResponseBody response
    let request
         = setRequestMethod "POST"
         $ setRequestPath "/game/aa/player/1"
         $ setRequestHeaders [("Content-Type","application/json+list")]
         $ setRequestBodyLBS body
         $ request'
    response <- httpLBS request
    print response
    --
    -- main
  -- PIRMAS ZINGSNIS --------------------------------------------------------------------
    -- request' <- parseRequest "http://tictactoe.homedir.eu"
    -- let request
    --      = setRequestMethod "GET"
    --      $ setRequestPath "/game/nespesiuAntDeadline/player/2"
    --      $ setRequestHeaders [("Accept","application/json+list")]
    --      $ request'
    -- response <- httpLBS request
    -- print response
    --
    -- request' <- parseRequest "http://tictactoe.homedir.eu"
    -- -- let body = L8.pack $ makeMove ""
    -- let body = L8.pack $ makeMove $ L8.unpack $ getResponseBody response
    -- let request
    --      = setRequestMethod "POST"
    --      $ setRequestPath "/game/nespesiuAntDeadline/player/2"
    --      $ setRequestHeaders [("Content-Type","application/json+list")]
    --      $ setRequestBodyLBS body
    --      $ request'
    -- response <- httpLBS request
    -- print response

-- play :: IO() -> In/t -> Int
-- play moves = do
  -- print moves

makeMove :: String -> String
makeMove "" = "[[\"x\", 1, \"y\",   1, \"v\", \"x\"]]"
makeMove msg =
  let
    moves = parse msg
    status = defend moves
    nextMove = if status /= "free"
      then init msg ++ "," ++ status ++ "]"
      -- then init msg ++ "," ++ tail status
      else init msg ++ "," ++ (makeRandomMove moves 0 0) ++ "]"
        -- makeRandomMove moves
  in
    nextMove

makeRandomMove :: Moves -> Int -> Int -> String
makeRandomMove moves 3 3 = "no moves left"
makeRandomMove moves x 3 = makeRandomMove moves (x+1) 0
makeRandomMove moves x y =
  let
    l = length $ filter (\(a,b,player) -> a == x && b == y) moves
    nextMove = if l == 0
      then "[\"x\"," ++ (intToString x) ++ ", \"y\"," ++ (intToString y) ++ ", \"v\", \"" ++ (charToString botPlayer) ++ "\"]"
      else makeRandomMove moves x (y+1)
  in
    nextMove

defend :: Moves -> String
defend moves =
  let
    rw = checkRow moves 2
    cl = checkColumn moves 2
    dg = checkDiagonals moves 2
    nextMove = if rw /= "free"
          then rw
          else if cl /= "free"
            then cl
              else if dg /= "free"
                then dg
                else "free"
  in
    nextMove
-- makeMove (msg:rest) =  rest
message :: String
message = "[[\"x\", 2, \"y\",   2, \"v\", \"x\"], [\"x\",  1,   \"y\",   1, \"v\", \"x\"], [\"x\", 2,  \"y\",  1,   \"v\", \"x\"],  [\"x\",   1,  \"y\",  0, \"v\", \"o\"],   [\"x\",  0,  \"y\",   2, \"v\", \"o\"]]"

message1 :: String
message1 = "[[\"x\", 0, \"y\",  1, \"v\", \"x\"], [\"x\",  1,   \"y\",   0, \"v\", \"o\"], [\"x\", 0,  \"y\",  2,   \"v\", \"x\"],  [\"x\",   1,  \"y\",  1, \"v\", \"o\"]]"

message2 :: String
message2 = "[[\"x\", 2, \"y\",  0, \"v\", \"o\"], [\"x\",  0,   \"y\",   0, \"v\", \"x\"], [\"x\", 1,  \"y\",  1,   \"v\", \"x\"],  [\"x\",   2,  \"y\",  2, \"v\", \"o\"]]"

message3 :: String
message3 = "[[\"x\", 1, \"y\",  1, \"v\", \"x\"], [\"x\",  0,   \"y\",   0, \"v\", \"o\"], [\"x\", 1,  \"y\",  2,   \"v\", \"x\"],  [\"x\",   2,  \"y\",  0, \"v\", \"o\"]]"

message4 :: String
message4 = "[[\"x\", 1, \"y\",  1, \"v\", \"x\"], [\"x\",  1,   \"y\",   2, \"v\", \"o\"], [\"x\", 0,  \"y\",  1,   \"v\", \"x\"],  [\"x\",   2,  \"y\",  2, \"v\", \"o\"]]"

message5 :: String
message5 = "[[\"x\", 1, \"y\",  0, \"v\", \"x\"], [\"x\",  0,   \"y\",   2, \"v\", \"o\"], [\"x\", 2,  \"y\",  2,   \"v\", \"x\"],  [\"x\",   1,  \"y\",  1, \"v\", \"o\"]]"

message6 :: String
message6 = "[[\"x\", 0, \"y\",  1, \"v\", \"x\"], [\"x\",  0,   \"y\",   0, \"v\", \"o\"], [\"x\", 2,  \"y\",  1,   \"v\", \"x\"],  [\"x\",   1,  \"y\",  1, \"v\", \"o\"],  [\"x\",   0,  \"y\",  2, \"v\", \"o\"],  [\"x\",   1,  \"y\",  0, \"v\", \"x\"],  [\"x\",   1,  \"y\",  2, \"v\", \"o\"], [\"x\",   2,  \"y\",  0, \"v\", \"o\"]]"
-- message :: String


move :: String
move = "[[\"x\", 1, \"y\",   2, \"v\", \"x\"]]"

botPlayer :: Char
botPlayer = 'x'

enemy :: Char
enemy = 'o'


type Move = (Int, Int, Char)
type Moves = [Move]

intToString :: Int -> String
intToString 0 = "0"
intToString 1 = "1"
intToString 2 = "2"

charToString :: Char -> String
charToString c = [c]


findEmptyRowCell :: Moves -> Int
findEmptyRowCell a =
  let
    first = a !! 0
    (x,y,p) = first
    second = a !! 1
    (x1,y1,p1) = second
    coordSum = y + y1
    coord = if coordSum == 1
      then 2
      else if coordSum == 2
        then 1
        else 0
  in
    coord

findEmptyColumnCell :: Moves -> Int
findEmptyColumnCell a =
  let
    first = a !! 0
    (x,y,p) = first
    second = a !! 1
    (x1,y1,p1) = second
    coordSum = x + x1
    coord = if coordSum == 1
      then 2
      else if coordSum == 2
        then 1
        else 0
  in
    coord

countScore :: Moves -> Char -> Int
countScore moves p = length $ filter (\(x,y,player) -> player == p) moves

checkRow :: Moves -> Int -> String
checkRow moves (-1) = "free"
checkRow moves l =
  let
    row = filter (\(x,y,player) -> x == l) moves
    countEnemy = countScore row enemy
    countBotPlayer = countScore row botPlayer
    nextMove = if countEnemy == 2 && countBotPlayer == 0
      then "[\"x\"," ++ (intToString l) ++ ", \"y\"," ++ (intToString $ findEmptyRowCell row) ++ ", \"v\", \"" ++ (charToString botPlayer) ++ "\"]"
      else checkRow moves (l-1)
  in
    nextMove

checkColumn :: Moves -> Int -> String
checkColumn moves (-1) = "free"
checkColumn moves l =
  let
    column = filter (\(x,y,player) -> y == l) moves
    countEnemy = countScore column enemy
    countBotPlayer = countScore column botPlayer
    nextMove = if countEnemy == 2 && countBotPlayer == 0
      then "[\"x\"," ++ (intToString $ findEmptyColumnCell column) ++ ", \"y\"," ++ (intToString l) ++ ", \"v\", \"" ++ (charToString botPlayer) ++ "\"]"
      else checkColumn moves (l-1)
  in
    nextMove

checkDiagonals :: Moves -> Int -> String
checkDiagonals moves l =
  let
    diagonal1 = concat $ filter (not . null) $ getDiagonal1 moves l
    diagonal2 = concat $ filter (not . null) $ getDiagonal2 moves l
    a1 = checkDiagonal diagonal1
    a2 = checkDiagonal diagonal2
    nextMove = if a1 == True
      then  "[\"x\"," ++ (intToString $ findEmptyRowCell diagonal1) ++ ", \"y\"," ++ (intToString $ findEmptyRowCell diagonal1) ++ ", \"v\", \"" ++ (charToString botPlayer) ++ "\"]"
      else if a2 == True
        then "[\"x\"," ++ (intToString $ 2 - (findEmptyRowCell diagonal2)) ++ ", \"y\"," ++ (intToString $ findEmptyRowCell diagonal2) ++ ", \"v\", \"" ++ (charToString botPlayer) ++ "\"]"
        else "free"
  in
    nextMove

checkDiagonal :: Moves -> Bool
checkDiagonal diagonal =
  let
    countEnemy = countScore diagonal enemy
    countBotPlayer = countScore diagonal botPlayer
    result = if countEnemy == 2 && countBotPlayer == 0
      then True
      else False
  in
    result

getDiagonal1 :: Moves -> Int -> [Moves]
getDiagonal1 moves (-1) = []
getDiagonal1 moves l =
  let
    diag = filter (\(x,y,player) -> x == l && y == l) moves
    sm = getDiagonal1 moves (l-1)
  in
    diag:sm

getDiagonal2 :: Moves -> Int -> [Moves]
getDiagonal2 moves (-1) = []
getDiagonal2 moves l =
  let
    diag = filter (\(x,y,player) -> x == l && y == abs(l-2)) moves
    sm = getDiagonal2 moves (l-1)
  in
    diag:sm

--Parsing into tuples

parse :: String -> Moves
parse ('[':rest) = (parseList . trim) rest
parse _ = error "Can't parse, not a list"

parseList :: String -> Moves
parseList "]" = []
parseList str =
  let
    (tuple, rest) = parseToTuple str
    list = readSeparator rest
    tupleBefore = parseList list
  in
    tuple:tupleBefore

parseToTuple :: String -> (Move, String)
parseToTuple ('[':rest) =
  let
    (x, restx) = parseDigit rest
    (y, resty) = parseDigit restx
    prefix = readPrefix resty
    separator = readSeparator prefix
    (p, restp) = readPlayer separator
  in
    case restp of
      (']':t) -> ((x, y, p), t)
      _       -> error "Tuple without closing bracket"

parseDigit :: String -> (Int, String)
parseDigit str =
  let
    prefix = readPrefix str
    separator1 = readSeparator prefix
    (player, rest) = readDigit separator1
    separator2 = readSeparator rest
  in
    (player, separator2)

--Parsing symbols from string

trim :: [Char] -> [Char]
trim [] = []
trim (' ':xs) = trim xs
trim (x:xs) =
  let
    rest = trim xs
  in
    x:rest

readPrefix :: String -> String
readPrefix ('"':coord:'"':rest) = rest
readPrefix _ = error "Prefix expected"

readDigit :: String -> (Int, String)
readDigit ('0':rest) = (0, rest)
readDigit ('1':rest) = (1, rest)
readDigit ('2':rest) = (2, rest)
readDigit _ = error "Digit expected"

readSeparator :: String -> String
readSeparator (',':rest) = rest
readSeparator "]" = "]"
readSeparator _ = error "Separator expected"

readPlayer :: String -> (Char, String)
readPlayer ('"': 'x' : '"': rest) = ('x', rest)
readPlayer ('"': 'o' : '"': rest) = ('o', rest)
readPlayer _ = error "Player expected"
