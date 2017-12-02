#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE RecordWildCards #-}
import Utils

solve1 :: [Int] -> Int
solve1 xs = getSum
      . foldMap (Sum . fst)
      . filter (uncurry (==))
      . take (length xs)
      $ zip (cycle xs) (tail (cycle xs))

main :: IO ()
main = getContents >>= print . solve1 . map digitToInt . filter isDigit
