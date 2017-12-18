#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils

solve n = head . tail . head . drop 2017 $ unfoldr go (1, [0]) where
  go (i, buf) = Just (buf, (i+1, buf'))
    where buf' = (i :)  . take i . drop (n+1) . cycle $ buf

--solve2 = head . tail . dropWhile (/= 0) . cycle . head . drop 50000000 . solve

--solve2 n =  [1..]

main :: IO ()
main = do
  n <- read <$> readFile "input.txt"
  print $ solve 3
  print $ solve n
--  print $ solve2 n
