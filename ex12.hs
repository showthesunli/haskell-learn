type Board = [Int]

board :: Board
board = [5, 4, 3, 2, 1]

type Player = String

player1 :: Player
player1 = "player1"

player2 :: Player
player2 = "player2"

takeStar :: Int -> Board -> Board
takeStar n board = if rest > 0 then init board ++ [rest] else init board
  where
    rest = last board - n

isWin :: Board -> Bool
isWin = null

takeN :: IO Int
takeN = do
  n <- getLine
  return (read n :: Int)

takeTurn :: Player -> Board -> IO (Bool, Board)
takeTurn p b = do
  n <- takeN
  let res = takeStar n b
  return (isWin res, res)

takeP :: [Player] -> [Player]
takeP ps = foldr (:) (takeP ps) ps

play :: [Player] -> Board -> IO ()
play (p : ps) b = do
  putStrLn ("board is: " ++ (show b))
  putStr (p ++ "'s turn, please input a number: ")
  (win, nb) <- takeTurn p b
  if win
    then print (p ++ " win!")
    else play ps nb

began :: IO ()
began = play (takeP [player1, player2]) board