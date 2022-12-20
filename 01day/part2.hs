import Data.List (group, groupBy)
import Data.Maybe (fromMaybe)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)
import Text.Read (readMaybe)

parseLines :: [String] -> [Int]
parseLines = map (fromMaybe 0 . readMaybe)

splitOnN :: (Eq a) => a -> [a] -> [[a]]
splitOnN _ [] = [[]]
splitOnN n (x : xs)
  | x == n = [] : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = splitOnN n xs

solution :: String -> Int
solution = sum . take 3 . map sum . splitOnN 0 . parseLines . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print (solution contents)
  hClose handle