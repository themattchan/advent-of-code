#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import qualified Data.Map as M

data Dir = U | R | D | L deriving (Show, Eq, Enum)

turnLeft, turnRight :: Dir -> Dir
turnLeft U = L
turnLeft x = pred x

turnRight L = U
turnRight x = succ x

data Coord = C {-#UNPACK#-}!Int {-#UNPACK#-}!Int deriving (Show, Ord, Eq)

data CoordSt = {- Clean (not in map) | -} Weakened | Infected | Flagged deriving (Show, Ord, Eq, Enum)

type Grid = M.Map Coord CoordSt

step :: Coord -> Dir -> Coord
step (C x y) = \case
  U -> C x (y+1)
  L -> C (x-1) y
  D -> C x (y-1)
  R -> C (x+1) y

turnBySt :: Maybe CoordSt -> Dir -> Dir
turnBySt = \case
  Nothing       -> turnLeft
  Just Weakened -> id
  Just Infected -> turnRight
  Just Flagged  -> turnRight . turnRight

data State = State {-# UNPACK #-}!Coord !Dir !Grid {-# UNPACK #-}!Int

getInfectMoveCount (State _ _ _ i) = i

type GridUpdater = Coord -> Grid -> (Grid, Int)

updateGrid1 :: GridUpdater
updateGrid1 c g = case M.lookup c g of
  Just Infected -> (M.delete c g, 0)
  _             -> (M.insert c Infected g, 1)

updateGrid2 :: GridUpdater
updateGrid2 c g = case M.lookup c g of
  Just Flagged  -> (M.delete c g, 0) -- clean
  Nothing       -> (M.insert c Weakened g, 0)
  Just Weakened -> (M.adjust succ c g, 1)
  _             -> (M.adjust succ c g, 0)

move :: GridUpdater -> State -> State
move rule (State c d g infect) = (State c' d' g' (infect + infected))
  where
    d' = turnBySt (M.lookup c g) d
    (g', infected) = rule c g
    c' = step c d'

evolve :: GridUpdater -> String -> [State]
evolve rule = iterate (move rule) . mkInitState . readInput
  where
    mkInitState g = State (C 0 0) U g 0

--------------------------------------------------------------------------------

readInput :: String -> Grid
readInput s = M.fromList [(C x y, Infected) | (y, xs) <- indexed, (x, c) <- xs, c == '#']
  where
    l = lines s
    h = length l
    w = length (head l)
    coords n = let n' = (n-1) `div` 2 in [-n'..n']
    indexed = zip (reverse (coords h)) . map (zip (coords w)) $ l

solve :: Int -> [State] -> Int
solve n ss = getInfectMoveCount $ ss !! n

test = "..#\n#..\n..."

main :: IO ()
main = do
  xs <- readFile "input.txt"

  putStrLn $ "PART 1"
  timed $ print $ solve 70    (evolve updateGrid1 test) == 41
  timed $ print $ solve 10000 (evolve updateGrid1 test) == 5587
  timed $ print $ solve 10000 (evolve updateGrid1 xs)

  putStrLn $ "\nPART 2"
  timed $ print $ solve 100      (evolve updateGrid2 test) == 26
  timed $ print $ solve 10000000 (evolve updateGrid2 test) == 2511944
  timed $ print $ solve 10000000 (evolve updateGrid2 xs)
