import Data.List (group, nub, tails, transpose)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile, print)

windows :: Int -> [a] -> [[a]]
windows n xs = transpose (take n (tails xs))

firstDistinct :: (Eq a) => Int -> [a] -> Int
firstDistinct n xs = length (head $ group $ map (\x -> length (nub x) == length x) $ windows n xs) + n

solution :: String -> (Int, Int)
solution xs = (first, second)
  where
    first = firstDistinct 4 xs
    second = firstDistinct 14 xs

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ solution contents
  hClose handle