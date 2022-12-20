import Data.Array (Array, assocs, bounds, listArray, (!), (//))
import qualified Data.Bifunctor
import Data.Char (ord)
import Data.Set (Set, empty, fromList, member, toList, union, unions)
import Debug.Trace (trace)

type Index = (Int, Int)

start :: Int
start = ord 'S' - ord 'a'

finish :: Int
finish = ord 'E' - ord 'a'

parseMap :: String -> Array Index Char
parseMap xs = listArray ((1, 1), (height, width)) (concat l)
  where
    l = lines xs
    height = length l
    width = length $ head l

charsToInts :: Array Index Char -> Array Index Int
charsToInts = ((subtract (ord 'a') . ord) <$>)

neighbors :: Array Index Int -> Set Index -> Index -> Set Index
neighbors arr seen i@(x, y) =
  let ((minx, miny), (maxx, maxy)) = bounds arr
   in fromList $
        filter
          ( \xy@(xx, yy) ->
              not (member xy seen)
                && xx
                >= minx
                  && xx <= maxx
                  && yy
                >= miny
                  && yy <= maxy
                  && ( (arr ! (xx, yy) == finish && arr ! i >= 24)
                         || (arr ! (xx, yy) /= finish && (arr ! (xx, yy) - arr ! i) <= 1)
                     )
          )
          $ map (Data.Bifunctor.bimap (x +) (y +)) [(0, 1), (0, -1), (1, 0), (-1, 0)]

bfs :: Array Index Int -> Set Index -> [Index] -> Int -> Int
bfs arr seen queue n
  | null queue = 9999999
  | any (\i -> arr ! i == finish) seen = n - 1
  | otherwise = bfs arr (seen `union` fromList queue) (toList $ unions $ map (neighbors arr seen) queue) (n + 1)

shortestPath :: Array Index Int -> Index -> Int
shortestPath arr startingPoint = bfs (arr // [(startingPoint, 0)]) empty [startingPoint] 0

solution :: String -> (Int, Int)
solution content = (first, second)
  where
    arr = charsToInts $ parseMap content
    s = fst $ head $ filter (\t -> snd t == start) (assocs arr)
    allAs = map fst $ filter (\t -> snd t == 0) (assocs arr)
    first = shortestPath arr s
    second = minimum $ map (shortestPath arr) allAs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (first, second) = solution contents
  putStrLn $ "Part 1: " ++ show first
  putStrLn $ "Part 2: " ++ show second