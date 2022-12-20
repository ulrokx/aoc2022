import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (elemIndex, sort)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)

data Packet = Base Int | List [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare (Base l) (Base r) = compare l r
  compare l r@(Base _) = compare l (List [r])
  compare l@(Base _) r = compare (List [l]) r
  compare (List l) (List r) = compare l r

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser
      ( \input -> do
          (input', x) <- p input
          Just (input', f x)
      )

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser
      ( \input -> do
          (input', f) <- p1 input
          (input'', a) <- p2 input'
          Just (input'', f a)
      )

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser a) <|> (Parser b) = Parser $ \input -> a input <|> b input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser [Char]
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser
    ( \input -> do
        let (token, rest) = span f input
        Just (rest, token)
    )

ws :: Parser String
ws = spanP isSpace

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser
    ( \input -> do
        (input', xs) <- p input
        if null xs
          then Nothing
          else Just (input', xs)
    )

base :: Parser Packet
base = r <$> notNull (spanP isDigit)
  where
    r = Base . (read :: String -> Int)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

list :: Parser Packet
list = List <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) packet

packet :: Parser Packet
packet = base <|> list

countOrdered :: [Packet] -> Int -> Int
countOrdered [] _ = 0
countOrdered (l : r : xs) idx = (idx * (if l < r then 1 else 0)) + countOrdered xs (idx + 1)
countOrdered [_] _ = error "odd number of things :P"

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let packetStrings = filter (not . null) $ lines contents
  let packets = mapMaybe (fmap snd . runParser packet) packetStrings
  let count = countOrdered packets 1
  putStrLn $ "Part 1: " ++ show count
  let dividers@[firstDivider, secondDivider] = mapMaybe (fmap snd . runParser packet) ["[[2]]", "[[6]]"]
  let packetsWithDividers = packets ++ dividers
  let sortedPackets = sort packetsWithDividers
  let indexes = mapMaybe (`elemIndex` sortedPackets) dividers
  putStrLn $ "Part 2: " ++ show (product $ map (+ 1) indexes)