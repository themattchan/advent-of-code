#! /usr/bin/env runhaskell -i../../
import Utils
import Data.Semigroup (Max(..))

data Dir = N | NE | SE | S | SW | NW
  deriving (Show, Ord, Eq)

instance Read Dir where
  readsPrec _ ('n':'e':xs) = [(NE,xs)]
  readsPrec _ ('s':'e':xs) = [(SE,xs)]
  readsPrec _ ('s':'w':xs) = [(SW,xs)]
  readsPrec _ ('n':'w':xs) = [(NW,xs)]
  readsPrec _ ('n'    :xs) = [(N,xs)]
  readsPrec _ ('s'    :xs) = [(S,xs)]

-- http://catlikecoding.com/unity/tutorials/hex-map/part-1/
type HexCoord = (Sum Int, Sum Int)
 -- x and z.
 -- y is recoverable, see below

mkCoord ns nwse = (Sum ns, Sum nwse)

toCoord N  = mkCoord 0 1
toCoord S  = mkCoord 0 (-1)
toCoord NE = mkCoord 1 0
toCoord SW = mkCoord (-1) 0
toCoord SE = mkCoord 1 (-1)
toCoord NW = mkCoord (-1) 1

getDist (Sum x, Sum z) = maximum [abs x, abs y, abs z]
  where y = (-x) + (-z)

solve2 :: [Dir] -> (Int, Int)
solve2 = bimap getDist getMax . foldl go (mempty, mempty)
  where
    go (coord, m) a = let x = coord <> toCoord a
                      in (x, m <> Max (getDist x))

main = do
  ds <- readFile "input.txt"
  let dirs = read $ "[" ++ ds ++ "]"
  print (solve2 dirs)
