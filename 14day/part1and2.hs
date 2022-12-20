{-# LANGUAGE OverloadedStrings #-}

import Data.Array.Unboxed
  ( UArray,
    array,
    assocs,
    bounds,
    elems,
    (!),
    (//),
  )
import Data.List (sort, tails, transpose)
import qualified Data.Text as T
import Debug.Trace (trace)

caveMinX = 400

caveMaxX = 600

caveMinY = 0

caveMaxY = 200

type Coord = (Int, Int)

bottom :: UArray Coord Char -> Int
bottom = snd . snd . bounds

windows :: Int -> [a] -> [[a]]
windows n xs = filter ((== n) . length) $ transpose (take n (tails xs))

fillIn :: [(Int, Int)] -> [((Int, Int), Char)]
fillIn [(bx, by), (ex, ey)]
  | bx == ex = let [y1, y2] = sort [by, ey] in [((bx, y), 'X') | y <- [y1 .. y2]]
  | otherwise = let [x1, x2] = sort [bx, ex] in [((x, by), 'X') | x <- [x1 .. x2]]
fillIn _ = error "bad fillIn"

createRockArray :: [[Coord]] -> Int -> UArray Coord Char
createRockArray [] depth = array ((300, 0), (700, depth)) [((x, y), ' ') | x <- [300 .. 700], y <- [0 .. depth]]
-- createRockArray [] depth = array ((480, 0), (520, depth)) [((x, y), ' ') | x <- [480 .. 520], y <- [0 .. depth]]
createRockArray (x : xs) d = createRockArray xs d // concatMap fillIn (windows 2 x)

parseRocks :: String -> [[Coord]]
parseRocks content = map (map (\c -> read ("(" ++ T.unpack c ++ ")") :: Coord) . T.splitOn " -> " . T.pack) (lines content)

findDrop :: UArray Coord Char -> Coord -> Coord
findDrop cave i@(x, y)
  | y == (snd . snd) (bounds cave) = (x, y)
  | cave ! (x, y + 1) == ' ' = findDrop cave (x, y + 1)
  | cave ! (x - 1, y + 1) == ' ' = findDrop cave (x - 1, y + 1)
  | cave ! (x + 1, y + 1) == ' ' = findDrop cave (x + 1, y + 1)
  | otherwise = i

dropSand :: UArray Coord Char -> UArray Coord Char
dropSand cave = cave // [(findDrop cave (500, 0), 'O')]

atBottom :: UArray Coord Char -> Bool
atBottom arr = any (\((x, y), c) -> y == bottom arr && c == 'O') $ assocs arr

full :: UArray Coord Char -> Bool
full = (== 'O') . (! (500, 0))

largestY :: [[Coord]] -> Int
largestY = maximum . concatMap (map snd)

mapSnd :: [(Int, Int)] -> [Int]
mapSnd = map snd

main :: IO ()
main = do
  content <- readFile "input.txt"
  let parsed = parseRocks content
  let depth = (+ 1) $ largestY parsed
  let rocks = createRockArray parsed depth
  let dropped = until atBottom dropSand rocks
  let filled = until full dropSand rocks
  let amount = subtract 1 $ length $ filter (== 'O') $ elems dropped
  putStrLn $ "Part 1: " ++ show amount
  let amountToFill = length $ filter (== 'O') $ elems filled
  putStrLn $ "Part 2: " ++ show amountToFill