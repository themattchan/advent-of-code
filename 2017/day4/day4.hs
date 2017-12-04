#! /usr/bin/env runhaskell -i../../
import Utils

solve clean = length . filter (\xs -> length xs == length (clean xs))

main :: IO ()
main = do
  pp <- map words . lines <$> getContents
  print $ solve nub pp
  print $ solve (nub . map sort) pp
