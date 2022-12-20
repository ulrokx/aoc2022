import Data.List (nub)
import qualified Data.Set as S (fromList, size)

type Move = (Int, Int)

type Coord = (Int, Int)

headMoves :: String -> [Move]
headMoves moves = do
  dirString : _ : magString <- lines moves
  let mag = read magString :: Int
  let dir = case dirString of
        'U' -> up
        'D' -> down
        'L' -> left
        'R' -> right
        _ -> error "invalid direction"
  replicate mag dir

headPath :: [Move] -> [Coord]
headPath = scanl (\(x, y) (xx, yy) -> (x + xx, y + yy)) (0, 0)

followHead :: [Coord] -> [Coord]
followHead = tail . scanl follow (0, 0)

follow :: Coord -> Coord -> Coord
follow currTail@(cx, cy) currHead@(hx, hy) = newTail
  where
    diff@(dx, dy) = zipBoth2 (-) currHead currTail
    mags@(mx, my) = both abs diff
    moveX = if dx > 0 then right else left
    moveY = if dy > 0 then up else down
    newTail
      | (mx + my) > 2 = step moveX . step moveY $ currTail
      | mx > 1 = step moveX currTail
      | my > 1 = step moveY currTail
      | otherwise = currTail

-- we can use iterate to track the n-th rope segment, as each
-- time followHead is called, it is called on its parent rope segment
solveWithLength :: Int -> String -> Int
solveWithLength length = S.size . S.fromList . (!! length) . iterate followHead . headPath . headMoves

solution :: String -> (Int, Int)
solution input = (first, second)
  where
    first = solveWithLength 1 input
    second = solveWithLength 9 input

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solution contents

up = (0, 1)

down = (0, -1)

right = (1, 0)

left = (-1, 0)

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)

zipBoth2 :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
zipBoth2 f (a, b) (c, d) = (f a c, f b d)

step :: Move -> Coord -> Coord
step (dx, dy) (x, y) = (x + dx, y + dy)
