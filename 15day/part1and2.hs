{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List (nub, sort)
import qualified Data.Text as T

yValue = 2000000

-- yValue = 10

type Coord = (Int, Int)

type Range = (Int, Int)

type Sensor = (Coord, Coord)

readDigit :: String -> Int
readDigit = read . takeWhile (\c -> isDigit c || (== '-') c)

manhattan :: Sensor -> Int
manhattan ((sx, sy), (bx, by)) = abs (sx - bx) + abs (sy - by)

getCoords :: T.Text -> Sensor
getCoords text = ((sx, sy), (bx, by))
  where
    [stext, btext] = map T.unpack $ T.splitOn ": " text
    sx = readDigit $ drop 12 stext
    sy = readDigit $ drop 2 $ dropWhile (/= 'y') stext
    bx = readDigit $ drop 23 btext
    by = readDigit $ drop 2 $ dropWhile (/= 'y') btext

parseSensors :: String -> [Sensor]
parseSensors = map c . lines
  where
    c = getCoords . T.pack

inYAxis :: Int -> [Sensor] -> [Range]
inYAxis y = concatMap (getRange y)

getRange :: Int -> Sensor -> [Range]
getRange y xy@((sx, sy), (bx, by)) =
  let m = manhattan xy
      diff = abs (y - sy)
      r = m - diff
   in [(sx - r, sx + r) | diff <= m]

mergeRanges :: [Range] -> [Range]
mergeRanges rs = foldl (\acc (b, e) -> let (bb, ee) = head acc in if b <= ee then (bb, max ee e) : tail acc else (b, e) : acc) [head rs] rs

count :: Int -> [Coord] -> [Range] -> Int
count y beacons = foldr (\(b, e) acc -> acc + ((e - b + 1) - length (filter (\bb -> bb >= b && bb <= e) bs))) 0
  where
    bs = map fst $ filter (\(_, yy) -> yy == y) beacons

countInRange :: Int -> Int -> [Coord] -> [Range] -> Int
countInRange r y beacons =
  foldr
    ( \(b, e) acc ->
        let br = max 0 b
            er = min r e
         in acc + ((er - br) - length (filter (\bb -> bb >= br && bb <= er) bs))
    )
    0
  where
    bs = map fst $ filter (\(_, yy) -> yy == y) beacons

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let sensors = parseSensors contents
  let inY = inYAxis yValue sensors
  let merged = mergeRanges $ sort inY
  let beacons = nub $ map snd sensors
  let c = count yValue beacons merged
  putStrLn $ "Part 1:" ++ show c
  let intervals = length $ takeWhile (== 4000000) $ map (\y -> countInRange 4000000 y beacons $ mergeRanges $ sort $ inYAxis y sensors) [0 .. 4000000]
  putStrLn $ "Part 2:" ++ show intervals
