{-# LANGUAGE OverloadedStrings #-}

import Data.Either (fromRight)
import Data.List (sort)
import qualified Data.Map as M (Map, elems, empty, insertWith, update, (!))
import qualified Data.Text as T
import Data.Text.Read (decimal)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

type Filesystem = M.Map T.Text Int

type Path = T.Text

type Command = T.Text

updateFs :: Path -> Int -> Filesystem -> Filesystem
updateFs = M.insertWith (+)

parse :: Filesystem -> [Path] -> [Command] -> Filesystem
-- we are out of commands to parse
parse fs _ [] = fs
parse fs stack cmds
  -- pop path from head of stack
  | isPrefix "$ cd .." = parse fs (tail stack) rest
  | isPrefix "$ cd /" = parse fs [""] rest
  -- get the new directory and append the new complete path to the stack
  | isPrefix "$ cd" =
    let newDir = T.drop 5 cmd
        parent = head stack
        newPath = T.concat [parent, "/", newDir]
     in parse fs (newPath : stack) rest
  -- we can ignore dir in the ls results
  | isPrefix "dir" = parse fs stack rest
  | otherwise =
    let sizeStr = head $ T.words cmd
        size = decimal sizeStr :: Either String (Int, T.Text)
        sizeInt = fst $ fromRight (0, "") size
        newFs = foldr (`updateFs` sizeInt) fs stack
     in parse newFs stack rest
  where
    cmd = head cmds
    rest = tail cmds
    isPrefix = flip T.isPrefixOf cmd

parseCommands :: [Command] -> Filesystem
parseCommands = parse M.empty [""]

getAnswers :: Filesystem -> (Int, Int)
getAnswers fs = (first, second)
  where
    elems = M.elems fs
    first = sum $ filter (<= 100000) elems
    needed = 30000000 - (70000000 - fs M.! "")
    second = head $ dropWhile (< needed) $ sort elems

solution :: String -> (Int, Int)
solution = getAnswers . parseCommands . map T.pack . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ solution contents
  hClose handle