#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import qualified Data.Set as S

data Dir = U | R | D | L deriving (Show, Eq, Enum)

-- instance Enum Dir where
--   toEnum n = [Up,Down,Left,Right] !! n `mod` 4
--   fromEnum e = elemIndex e [Up,Down,Left,Right]

turnLeft, turnRight :: Dir -> Dir
turnLeft U = L
turnLeft x = pred x

turnRight L = U
turnRight x = succ x

data Coord = C {-#UNPACK#-}!Int {-#UNPACK#-}!Int deriving (Show,Ord, Eq)

type State = (Coord, Dir, Grid, Int)

getInfectMoveCount (_,_,_,i) = i

move :: State -> State
move (c@(C x y), d, g, infect) = (c', d', g', infect')
  where
    isInfected = c `S.member` g

    d' | isInfected = turnRight d
       | otherwise = turnLeft d

    g' | isInfected = c `S.delete` g
       | otherwise = c `S.insert` g

    infect' | not isInfected = infect + 1
            | otherwise = infect

    c' = d' & \case
      U -> C x (y+1)
      L -> C (x-1) y
      D -> C x (y-1)
      R -> C (x+1) y

type Grid = S.Set Coord

readInput :: String -> Grid
readInput s = S.fromList [C x y | (y, xs) <- indexed, (x, c) <- xs, c == '#']
  where
    l = lines s
    h = length l
    w = length (head l)
    coords n = let n' = (n-1) `div` 2 in [-n'..n']
    indexed = zip (reverse (coords h)) . map (zip (coords w)) $ l


evolve :: String -> [State]
evolve = iterate move . mkInitState . readInput
  where
    mkInitState g = (C 0 0, U, g, 0)

solve1 :: Int -> [State] -> Int
solve1 n ss = getInfectMoveCount $ ss !! n

test = "..#\n#..\n..."

main :: IO ()
main = do
  print $ solve1 70 (evolve test)  == 41
  print $ solve1 10000 (evolve test)  == 5587
  xs <- readFile "input.txt"
  print $ solve1 10000 (evolve xs)
  print $ solve1 10000000 (evolve xs)
