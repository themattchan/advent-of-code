#! /usr/bin/env runhaskell -i../../
-- compile with
-- ghc day6.hs  -O2 -i../../
{-# LANGUAGE TypeApplications #-}

import qualified Data.Array as A
import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Utils
import Debug.Trace

main = do
  coords <- V.fromList . map (read @(Int,Int) . ("("<>).(<>")")) . lines <$> readFile "input.txt"
  -- for each coord from (1,1) to (500,500):
  -- compute closest input coord (index)
  -- compute map of { index : count }
  -- get rid of indices on edge:
  -- edges = (1,1) .. (1,500) + (1,1)..(500,1) + (1,500).. (500,500) + (500,1)..(500,500)
  -- find index with max count

  let dist (p1,p2) (q1,q2) = abs(p1-q1) + abs(p2-q2)

--  let closestPoint pt = V.minIndexBy (compare `on` dist pt) coords
  -- if there are multiple points with the same dist, nothing.
  let closestPoint pt
        | V.length ms > 1 = Nothing
        | otherwise = Just mi
        where
          ds = V.map (dist pt) coords
          mi = V.minIndex ds
          ms = V.elemIndices (ds V.! mi) ds

  let isEdge (i,j) = i == 1 || i == 500 || j == 1 || j == 500
  let grid = [(c, closestPoint c) | i <- [1..500], j <- [1..500], let c = (i,j)]
  let (edges, rest) = partition (isEdge . fst) grid
  let edgeIndices = (nub . mapMaybe snd) edges
  let indexFreqs = (map (head &&& length) . group . sort . mapMaybe snd) rest
  let best1 = maximumBy (compare `on` snd) $ filter ((`notElem` edgeIndices) . fst) indexFreqs
  print best1
