import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

type Stacks = [[Int]]

parseStacks :: [String] -> Stacks
parseStacks stacks= map parse [0..7]
    where parse 

solution :: String -> String
solution = undefined

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ solution contents
  hClose handle