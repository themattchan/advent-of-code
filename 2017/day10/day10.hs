#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE LambdaCase, BangPatterns #-}
import Utils
import Debug.Trace
import Data.Word

solve1 :: [Int] -> [Int] -> Int
solve1 xs = product . take 2 . tidy . foldl go (xs,0,0)
  where
    n = length xs

    -- rotate elements such that head of l is at position d
    tidy (l,d,_) = take n $ drop (n-d) $ cycle l

    -- head of the list is always the current position
    -- d tracks the current position as an index in the original ordering
    go (l,d,s) x
      = let (l1,l2) = splitAt x l
            l' = take n $ drop (x+s) $ cycle $ reverse l1 ++ l2
            d' = (d + x + s) `mod` n
        in (l', d', s+1)

main :: IO ()
main = do
  lengths <- read . ("["++) . (++"]") <$> readFile "input.txt"
  print $ solve [0..4] [3,4,1,5] -- 12
  print $ solve [0..255] lengths
