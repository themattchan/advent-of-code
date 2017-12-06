#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns, PatternGuards #-}
import Utils
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V

solve = findSeen . iterate redistribute

findSeen = go M.empty 0
  where
    go !seen !steps (x:xs)
      | Just lastSeen <- x `M.lookup` seen
      = (steps, steps-lastSeen)
      | otherwise
      = go (M.insert x steps seen) (steps+1) xs

redistribute xs = xs'
  where
    len = V.length xs
    mi = V.maxIndex xs
    e = xs V.! mi

    adds = [(i `mod` len, 1) | i <- [mi+1 .. mi+e] ]
    xs' = V.accum (+) (xs V.// [(mi,0)]) adds

main = do
  input <- V.fromList . map read . words <$> readFile "input.txt"
  print $ solve input
