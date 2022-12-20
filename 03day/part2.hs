import Data.Char (isAsciiLower, ord)
import Data.List (foldr1, intersect, unfoldr)
import Data.Set (empty, fromList, intersection, toList, union)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . unfoldr (Just . splitAt n)

score :: Char -> Int
score c
  | isAsciiLower c = (ord c - ord 'a') + 1
  | otherwise = (ord c - ord 'A') + 27

solve :: [String] -> Int
solve = score . head . toList . foldr1 intersection . map fromList

solution :: String -> Int
solution = sum . map solve . chunk 3 . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  print $ solution content
  hClose handle