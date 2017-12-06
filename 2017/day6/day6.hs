#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns, PatternGuards #-}
import Utils
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V

import Debug.Trace
solve :: [Int] -> (Int, Int)
solve = findRepeat . iterate redistribute . V.fromList

findRepeat :: (Ord a) => [a] -> (Int, Int)
findRepeat = go M.empty 0
  where
    go !seen !steps (x:xs)
      | Just lastSeen <- x `M.lookup` seen
      = (steps, steps-lastSeen)
      | otherwise
      = go (M.insert x steps seen) (steps+1) xs

redistribute :: V.Vector Int -> V.Vector Int
redistribute xs = xs'
  where
    len = V.length xs
    mi = V.maxIndex xs
    e = xs V.! mi

    adds = [(i `mod` len, 1) | i <- [mi+1 .. mi+e] ]
    xs' = V.accum (+) (xs V.// [(mi,0)]) adds

--------------------------------------------------------------------------------

solve' :: [Int] -> (Int, Int)
solve' = findRepeat' . uncurry iterate . (redistribute' . length &&& id)

findRepeat' :: (Ord a) => [a] -> (Int, Int)
findRepeat' = go 0 []
  where
    go !steps seen (x:xs)
      | Just relLastSeen <- x `elemIndex` seen
      = (steps, relLastSeen + 1)
      | otherwise
      = go (steps +1) (x:seen) xs

-- BROKEN
redistribute' :: Int -> [Int] -> [Int]
redistribute' len xs = traceShow (maxI,order xs') xx
  where
    xsi = zip [0..] xs
    (maxI, maxE) = maximumBy cmp xsi
    cmp (a,b)(c,d) = if c1 == EQ then compare c a else c1
      where c1 = compare b d
    (pre, (mi, m) : rest) = splitAt maxI xsi

    xs' = uncurry (++)
      -- TODO these numbers are wrong
        . ( ( drop (maxE - len)
            )
            ***
            ( take (max maxE (maxE - len))
            . drop (maxE - len)
            . concat
            )
          )
        . fromJust
        . uncons
        . iterate (map (fmap (+1)))
        $ rest ++ pre ++ [(mi, 0)]

    -- oldIndexOfZero = len-maxI-1
    -- newIdxOfZero = (oldIndexOfZero + (maxE - len)) `mod` len
    -- newIdxOfZero = (newIdxOfMi - maxI) `mod` len
    --order = uncurry (++) . swap . span ((/= 0) . fst)
    order = sortOn fst
    xx =snd <$> order xs'

test = [0,2,7,0]
main = do
  input <- map read . words <$> readFile "input.txt"
  -- (14029,2765)
  print $ solve input
--  print $ solve2 test
