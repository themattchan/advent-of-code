#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils

solve n = head . tail . head . drop 2017 $ unfoldr go (1, [0]) where
  go (i, buf) = Just (buf, (i+1, buf'))
    where buf' = (i :) . take i . drop (n+1) . cycle $ buf

-- let input be n
-- at iteration i: you will increase cursor by n mod i
-- new cursor = (prev + n) mod i
-- write to: (prev + n) mod i + 1
-- can never overwrite 0? is 0 always at index 0? proof?
insertInds n = scanl' go 0 [1..]
  where
    go prev i = (prev + n) `mod` i + 1

solve2 = last          -- most recent addition
       . elemIndices 1 -- all i's where you write to index 1
       . take 50000000
       . insertInds

main :: IO ()
main = do
  n <- read <$> readFile "input.txt"
  print $ solve 3
  print $ solve n
  print $ solve2 n
