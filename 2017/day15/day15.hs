#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns,
DeriveFunctor, TypeFamilies #-}
import Utils
import Debug.Trace
import Data.Bits
import Data.Word

largePrime :: Word64
largePrime = 2147483647

stepA1, stepB1 :: (w ~ Word64) => w -> w
stepA1 !x = mod (x * 16807) largePrime
stepB1 !x = mod (x * 48271) largePrime

solve1 :: (w ~ Word64) => w -> w -> w
solve1 = solve stepA1 stepB1 40000000

solve2 :: (w ~ Word64) => w -> w -> w
solve2 = solve (until mult4 stepA1) (until mult8 stepB1) 5000000
  where
    until p s !x = let !x' = s x in
                     if p x' then x' else until p s x'

    mult4 w = (w .&. 0x03) == 0
    mult8 w = (w .&. 0x07) == 0

solve :: (w ~ Word64) => (w -> w) -> (w -> w) -> w -> w -> w -> w
solve stepA stepB !rounds !seedA !seedB =
    go rounds 0 seedA seedB
  where
    go !n !sames !x !y
      | n == 0 = sames
      | otherwise =
        let !x' = stepA x
            !y' = stepB y
            !ss = if ((x' `xor`  y') .&. 0xffff) == 0 then 1 else 0
        in go (n-1) (sames + ss) x' y'

main :: IO ()
main = do
  [seedA, seedB] <- map (read . last . words) . lines <$> readFile "input.txt"
  print $ solve1 65 8921 == 588
  print $ solve1 seedA seedB

  print $ solve2 65 8921 == 309
  print $ solve2 seedA seedB
