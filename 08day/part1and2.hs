import Data.Char (digitToInt)
import Data.List (transpose)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

maximum' :: Ord a => a -> [a] -> a
maximum' d [] = d
maximum' _ [x] = x
maximum' d (x : xs) = max x $ maximum' d xs

type Grid = [[Int]]

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

enumerate2d :: (Grid -> (Int, Int) -> b) -> Grid -> [b]
enumerate2d f g = [f g (y, x) | (y, row) <- enumerate g, (x, tree) <- enumerate row]

visible :: Grid -> (Int, Int) -> Bool
visible g (row, col) =
  let val = (g !! row) !! col
   in any ((< val) . maximum' (-1)) [up, down, left, right]
  where
    t = transpose g
    horz = g !! row
    vert = t !! col
    up = take row vert
    down = drop (row + 1) vert
    left = take col horz
    right = drop (col + 1) horz

scenicScore :: Grid -> (Int, Int) -> Int
scenicScore g (row, col) =
  let extras = [above1, below1, left1, right1]
      sides = [above, below, left, right]
   in product $ zipWith (+) sides extras
  where
    t = transpose g
    rows = length g
    cols = length t
    horz = g !! row
    vert = t !! col
    val = (g !! row) !! col
    ltval = (< val)
    above = length $ takeWhile ltval $ reverse $ take row vert
    above1 = if above == row then 0 else 1
    below = length $ takeWhile ltval $ drop (row + 1) vert
    below1 = if below == (rows - row - 1) then 0 else 1
    left = length $ takeWhile ltval $ reverse $ take col horz
    left1 = if left == col then 0 else 1
    right = length $ takeWhile ltval $ drop (col + 1) horz
    right1 = if right == (cols - col - 1) then 0 else 1

parse :: String -> [[Int]]
parse = map2d digitToInt . lines

solution :: String -> (Int, Int)
solution input = (first, second)
  where
    g = parse input
    visibles = enumerate2d visible g
    scores = enumerate2d scenicScore g
    first = length $ filter id visibles
    second = maximum scores

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ solution contents
  hClose handle