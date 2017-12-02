#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE RecordWildCards #-}
import Utils

solve1 :: [Int] -> Int
solve1 xs = getSum
          . foldMap (Sum . fst)
          . filter (uncurry (==))
          . take (length xs)
          $ zip (cycle xs) (tail (cycle xs))

solve2 :: [Int] -> Int
solve2 xs = undefined

main :: IO ()
main = do
  ints <- map digitToInt . filter isDigit <$> getContents
  print $ solve1 ints
  print $ solve2 ints
