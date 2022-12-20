import Data.Char (isAsciiLower, ord)
import Data.Set (fromList, intersection, toList, union)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

score :: Char -> Int
score c
  | isAsciiLower c = (ord c - ord 'a') + 1
  | otherwise = (ord c - ord 'A') + 27

solve :: String -> Int
solve xs = sum $ map score $ toList $ a `intersection` b
  where
    half = length xs `div` 2
    [a, b] = map fromList $ (\(a, b) -> [a, b]) $ splitAt half xs

solution :: String -> Int
solution = sum . map solve . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  print $ solution content
  hClose handle