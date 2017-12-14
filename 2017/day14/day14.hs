#! /usr/bin/env runhaskell -i../../ -i../2017/day10
-- compile with
-- stack ghc  -- day14.hs  -O2 -i../../:../day10/

{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns,
DeriveFunctor #-}
--module Day14 where
import Utils
import qualified Day10 as KnotHash
import qualified Data.Set as S
import Data.Graph


solve1 :: String -> Int
solve1 input =
  sum [ count1's . KnotHash.solve2 $ input ++ show row
      | row <- [0..127]
      ]
  where
    count1's = sum . map go where
      go '0' = 0
      go '1' = 1
      go '2' = 1
      go '3' = 2
      go '4' = 1
      go '5' = 2
      go '6' = 2
      go '7' = 3
      go '8' = 1
      go '9' = 2
      go 'a' = 2
      go 'b' = 3
      go 'c' = 2
      go 'd' = 3
      go 'e' = 3
      go 'f' = 4

solve2 :: String -> Int
solve2 input = length . stronglyConnComp $ defragMap
  where
    defrag1Coords =
      [ (row, col)
      | row <- [0..127]
      , (col, bit) <- binaryRow row
      , bit == 1
      ]

    defrag1CoordsSet = S.fromList defrag1Coords

    defragMap =
      [ (u, u, filter (flip S.member defrag1CoordsSet) (neighbours u))
      | u <- defrag1Coords
      ]

    neighbours (row, col) = filter validCoord
      [              (row-1,col)
      , (row, col-1),           (row,col+1)
      ,              (row+1,col)
      ]

    validCoord (r,c) = 0 <= r && r <= 127 && 0 <= c && c <= 127

    binaryRow = zip [0..] . expand . KnotHash.solve2 . (input ++) . show

    expand = foldMap go
      where
      go '0' = [0,0,0,0]
      go '1' = [0,0,0,1]
      go '2' = [0,0,1,0]
      go '3' = [0,0,1,1]
      go '4' = [0,1,0,0]
      go '5' = [0,1,0,1]
      go '6' = [0,1,1,0]
      go '7' = [0,1,1,1]
      go '8' = [1,0,0,0]
      go '9' = [1,0,0,1]
      go 'a' = [1,0,1,0]
      go 'b' = [1,0,1,1]
      go 'c' = [1,1,0,0]
      go 'd' = [1,1,0,1]
      go 'e' = [1,1,1,0]
      go 'f' = [1,1,1,1]

main :: IO ()
main = do
  let test = "flqrgnkx-"
  let input = "jxqlasbh-"

  putStrLn ("TEST solve1: " ++ show (solve1 test == 8108))
  print . solve1 $ input

  putStrLn ("TEST solve2: " ++ show (solve2 test == 1242))
  print . solve2 $ input
