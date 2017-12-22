#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import qualified Data.Set as S
import qualified Data.Map as M

data Dir = U | R | D | L deriving (Show, Eq, Enum)

turnLeft, turnRight :: Dir -> Dir
turnLeft U = L
turnLeft x = pred x

turnRight L = U
turnRight x = succ x

data Coord = C {-#UNPACK#-}!Int {-#UNPACK#-}!Int deriving (Show,Ord, Eq)

step :: Coord -> Dir -> Coord
step (C x y) = \case
  U -> C x (y+1)
  L -> C (x-1) y
  D -> C x (y-1)
  R -> C (x+1) y

type State grid = (Coord, Dir, grid, Int)

getInfectMoveCount (_,_,_,i) = i

type Grid = S.Set Coord

readInput1 = S.fromList . readInput

move1 :: State Grid -> State Grid
move1 (c@(C x y), d, g, infect) = (c', d', g', infect')
  where
    isInfected = c `S.member` g

    d' | isInfected = turnRight d
       | otherwise = turnLeft d

    g' | isInfected = c `S.delete` g
       | otherwise = c `S.insert` g

    infect' | not isInfected = infect + 1
            | otherwise = infect

    c' = step d'

evolve1 :: String -> [State Grid]
evolve1 = iterate move1 . mkInitState . readInput1
  where
    mkInitState g = (C 0 0, U, g, 0)

--------------------------------------------------------------------------------

data CoordSt = {- Clean (not in map) | -} Weakened | Infected | Flagged  deriving (Show, Ord, Eq, Enum)

type Grid2 = M.Map Coord CoordSt

readInput2 :: String -> Grid2
readInput2 = M.fromList . flip zip (repeat Infected) . readInput

move2 :: State Grid2 -> State Grid2
move2 (c@(C x y), d, g, infect) = (c', d', g', infect')
  where
    cSt = c `M.lookup` g

    d' = case cSt of
      Nothing -> turnLeft d
      Just Weakened -> d
      Just Infected -> turnRight d
      Just Flagged -> turnRight (turnRight d)

    g' = case cSt of
      Just Flagged -> M.delete c g -- clean
      Nothing -> M.insert c Weakened g
      _ -> M.adjust succ c g

    infect' | cSt == Just Weakened = infect + 1
            | otherwise = infect

    c' = step d'

evolve2 :: String -> [State Grid2]
evolve2 = iterate move2 . mkInitState . readInput2
  where
    mkInitState g = (C 0 0, U, g, 0)

--------------------------------------------------------------------------------

readInput :: String -> [Coord]
readInput s = [C x y | (y, xs) <- indexed, (x, c) <- xs, c == '#']
  where
    l = lines s
    h = length l
    w = length (head l)
    coords n = let n' = (n-1) `div` 2 in [-n'..n']
    indexed = zip (reverse (coords h)) . map (zip (coords w)) $ l

solve :: Int -> [State g] -> Int
solve n ss = getInfectMoveCount $ ss !! n

test = "..#\n#..\n..."

main :: IO ()
main = do
  xs <- readFile "input.txt"

  putStrLn $ "PART 1"
  print $ solve 70    (evolve1 test) == 41
  print $ solve 10000 (evolve1 test) == 5587
  print $ solve 10000 (evolve1 xs)

  putStrLn $ "\nPART 2"
  print $ solve 100      (evolve2 test) == 26
  print $ solve 10000000 (evolve2 test) == 2511944
  print $ solve 10000000 (evolve2 xs)
