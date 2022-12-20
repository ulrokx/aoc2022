{-# LANGUAGE RecordPuns #-}

import Debug.Trace

data Instruction = Addx Int | Noop deriving (Show)

data CPU = CPU {register :: Int, ticksSpent :: Int, instructions :: [Instruction]}

instance Show CPU where
  show (CPU reg ticks _) = show reg ++ "|" ++ show ticks

tick :: CPU -> CPU
tick cpu@(CPU {register, ticksSpent, instructions = ((Addx x) : xs)})
  | ticksSpent == 0 = cpu {ticksSpent = 1}
  | otherwise = cpu {register = register + x, ticksSpent = 0, instructions = xs}
tick cpu@(CPU {instructions = (Noop : xs)}) = cpu {instructions = xs}
tick cpu@(CPU {instructions = []}) = cpu

parseInstructions :: String -> [Instruction]
parseInstructions l = do
  line <- lines l
  case words line of
    ["noop"] -> return Noop
    ["addx", num] -> return (Addx (read num :: Int))
    _ -> error "bad parse"

cycles :: [Int]
cycles = [20, 60, 100, 140, 180, 220]

render :: [CPU] -> [[Char]]
render cpus = [[display col (row + col) | col <- [1 .. 40]] | row <- [0, 40 .. 240]]
  where
    display :: Int -> Int -> Char
    display col i =
      let cpu = cpus !! i
       in if abs (register cpu - col) <= 1
            then 'X'
            else ' '

format :: [[Char]] -> String
format arr = unlines [unwords [[(arr !! row) !! col] | col <- [0 .. 39]] | row <- [0 .. 5]]

solution :: String -> (Int, String)
solution content = (first, second)
  where
    ticks = iterate tick (CPU {register = 1, ticksSpent = 0, instructions = parseInstructions content})
    first = sum $ map (\c -> c * register (ticks !! (c - 1))) cycles
    second = format $ render ticks

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (first, second) = solution contents
  putStrLn $ "Part 1:" ++ show first
  putStr $ "Part 2:\n" ++ second