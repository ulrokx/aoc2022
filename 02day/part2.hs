import Data.Char (ord)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import Text.Read ()

-- breaks the file into games
-- converts rock paper scissors into 0 1 2 respectively
parseLine :: String -> (Int, Int)
parseLine line = (oppn, men)
  where
    [opp, me] = map head $ words line
    oppn = ord opp - ord 'A'
    men = ord me - ord 'X'

-- logic for each game case
solve :: (Int, Int) -> Int
solve (0, n)
  | n == 0 = 3
  | n == 1 = 4
  | n == 2 = 8
solve (1, n)
  | n == 0 = 1
  | n == 1 = 5
  | n == 2 = 9
solve (2, n)
  | n == 0 = 2
  | n == 1 = 6
  | n == 2 = 7
solve _ = error "bad moves"

solution :: String -> Int
solution = sum . map (solve . parseLine) . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print (solution contents)
  hClose handle
