import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (for)
import Debug.Trace
import GHC.Tc.Validity (validDerivPred)

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

floydWarshall :: M.Map (String, String) Int -> [String] -> M.Map (String, String) Int
floydWarshall m vs = foldr fw m [(k, i, j) | k <- vs, i <- vs, j <- vs]
  where
    fw (k, i, j) acc =
      let newPath = acc M.! (i, k) + acc M.! (k, j)
       in if acc M.! (i, j) > newPath
            then M.adjust (const newPath) (i, j) acc
            else acc

makeAdjMatrix :: [String] -> M.Map String Valve -> M.Map (String, String) Int
makeAdjMatrix valves = M.foldrWithKey a initial
  where
    a origin (Valve _ ts) m = foldr (\t -> M.insert (origin, t) 1) m ts
    initial = foldr (\c@(x, y) acc -> let v = if x == y then 0 else 999999 in M.insert c v acc) M.empty [(x, y) | x <- valves, y <- valves]

currentFlow :: M.Map String Valve -> S.Set String -> Int
currentFlow valves = sum . S.map (\v -> flowRate $ valves M.! v)

maximumFlow :: Int -> S.Set String -> M.Map String Valve -> M.Map (String, String) Int -> Int
maximumFlow time usefulValves valves paths = f time "AA" 0 S.empty
  where
    f :: Int -> String -> Int -> S.Set String -> Int
    f minute location pressure open
      | usefulValves == open || minute == 0 = pressure + minute * currentFlow valves open
      | otherwise =
        let remaining = S.toList $ S.filter (\v -> (paths M.! (location, v) + 1) <= minute) $ usefulValves `S.difference` open
         in if null remaining
              then pressure + (minute * currentFlow valves open)
              else
                maximum
                  [ f (minute - d) next (pressure + (d * currentFlow valves open)) (S.insert next open)
                    | next <- remaining,
                      let d = paths M.! (location, next) + 1
                  ]

main :: IO ()
main = do
  content <- readFile "input.txt"
  let valves = parseValves content
  let vertices = M.keys valves
  let adjMatrix = makeAdjMatrix vertices valves
  let shortestPath = floydWarshall adjMatrix vertices
  let usefulValves = S.fromList $ M.keys $ M.filter ((/= 0) . flowRate) valves
  print $ "Part 1: " ++ show (maximumFlow 30 usefulValves valves shortestPath)
  let mine = S.elems $ S.powerSet usefulValves
  -- guessing that the optimal path will involve me and the elphant doing
  -- about the same number of moves, and there are 15 valves in my input
  let probablyOptimal = S.size usefulValves `div` 2
  let both = filter (\(a, b) -> S.size a == probablyOptimal) $ zip mine $ map (usefulValves `S.difference`) mine
  let best = maximum $ map (\(a, b) -> maximumFlow 26 a valves shortestPath + maximumFlow 26 b valves shortestPath) both
  print $ "Part 2: " ++ show best