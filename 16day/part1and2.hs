{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace (trace)

data Valve = Valve {flowRate :: Int, tunnels :: [String]} deriving (Show)

parseValves :: String -> M.Map String Valve
parseValves = foldr p M.empty . lines
  where
    p :: String -> M.Map String Valve -> M.Map String Valve
    p x acc =
      let valve = take 2 $ drop 6 x
          flow = read $ takeWhile isDigit $ drop 23 x
          tunnelz = splitOn ", " (let s = splitOn "valves " x in if length s == 1 then splitOn "valve " x !! 1 else s !! 1)
       in M.insert valve (Valve flow tunnelz) acc

currentFlow :: M.Map String Valve -> [String] -> Int
currentFlow valves = sum . map (\v -> flowRate $ valves M.! v)

findMax :: M.Map String Valve -> Int -> [String] -> String -> Int -> Int
findMax valves minute opened current total
  | minute == 30 = total
  | current `elem` opened || (flowRate (valves M.! current) == 0) = maximum [findMax valves (minute + 1) opened curr (total + currentFlow valves opened) | curr <- tunnels $ valves M.! current]
  | otherwise = findMax valves (minute + 1) (current : opened) current (total + currentFlow valves opened)

findMaxPressure :: M.Map String Valve -> Int
findMaxPressure valves = findMax valves 0 [] "AA" 0

main :: IO ()
main = do
  content <- readFile "example.txt"
  let valves = parseValves content
  let maxPressure = findMaxPressure valves
  putStrLn $ "Part 1: " ++ show maxPressure