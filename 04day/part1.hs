{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, split, unpack)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

solve :: String -> Int
solve xs
  | f1 <= s1 && f2 >= s2 || s1 <= f1 && s2 >= f2 = 1
  | otherwise = 0
  where
    splits = split (== '-') (pack xs)
    beginEnds = concatMap (split (== ',')) splits
    [f1, f2, s1, s2] = map ((read :: String -> Int) . unpack) beginEnds

solution :: String -> Int
solution = sum . map solve . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ solution contents
  hClose handle