{-# LANGUAGE RecordPuns #-}

import Data.List (sort, stripPrefix)
import Text.Read (Lexeme (Char), readMaybe)

data Monkey = Monkey {items :: [Integer], operation :: Integer -> Integer, test :: Integer -> Int, inspectCount :: Integer}

instance Show Monkey where
  show (Monkey items _ _ ic) = show items ++ " " ++ show ic

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs =
  let (front, back) = splitAt n xs
   in front : chunksOf n back

parseMonkey :: [String] -> Monkey
parseMonkey (_ : items : operation : test : throwTrue : throwFalse : _) =
  ( Monkey
      { items = parseItems items,
        operation = parseOperation operation,
        test = parseTest test throwTrue throwFalse,
        inspectCount = 0
      }
  )
parseMonkey _ = error "bad parse"

parseTest :: String -> String -> String -> (Integer -> Int)
parseTest test true false =
  let divisor = toInteger $ r $ drop 21 test
      trueMonkey = r $ drop 29 true
      falseMonkey = r $ drop 30 false
   in (\x -> if x `mod` divisor == 0 then trueMonkey else falseMonkey)
  where
    r = read :: String -> Int

parseOperation :: String -> (Integer -> Integer)
parseOperation op = case drop 23 op of
  ('*' : ' ' : num) -> p (*) num
  ('+' : ' ' : num) -> p (+) num
  _ -> error "bad operation"
  where
    p op str = maybe (\x -> x `op` x) (\x -> (`op` x)) (readMaybe str :: Maybe Integer)

parseItems :: String -> [Integer]
parseItems = (read :: String -> [Integer]) . (++ "]") . ('[' :) . drop 18

parseMonkeys :: String -> [Monkey]
parseMonkeys = map parseMonkey . chunksOf 7 . lines

div3 :: Integer -> Integer
div3 = (`div` 3)

divlcm :: Integer -> Integer
divlcm = (`mod` lcmm)

lcmm :: Integer
lcmm = 9699690

set :: (a -> a) -> Int -> [a] -> [a]
set f i xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

turn :: (Integer -> Integer) -> [Monkey] -> Int -> [Monkey]
turn relax ms i = set (\m@(Monkey {items, inspectCount}) -> m {items = [], inspectCount = inspectCount + toInteger (length items)}) i $ foldl item ms items
  where
    currMonkey@(Monkey {items, operation, test}) = ms !! i
    item monkeys worry =
      let newWorry = relax (operation worry)
          reciever = test newWorry
       in set (\m@(Monkey {items = is}) -> m {items = is ++ [newWorry]}) reciever monkeys

doRound :: (Integer -> Integer) -> [Monkey] -> [Monkey]
doRound relax monkeys = foldl (turn relax) monkeys [0 .. length monkeys - 1]

solution :: String -> (Integer, Integer)
solution content = (first, second)
  where
    monkeys = parseMonkeys content
    rounds = iterate (doRound divlcm) monkeys
    first = product $ take 2 $ reverse $ sort $ map inspectCount $ rounds !! 20
    second = product $ take 2 $ reverse $ sort $ map inspectCount $ rounds !! 10000

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (first, second) = solution contents
  putStrLn $ "Part 1:" ++ show first
  putStrLn $ "Part 2:" ++ show second