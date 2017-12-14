#! /usr/bin/env runhaskell -i../../ -i../2017/day10
-- compile with
-- stack ghc  -- day14.hs  -O2 -i../../:../day10/

{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns, DeriveFunctor #-}
--module Day14 where
import Utils
import qualified Day10 as KnotHash
import Data.Graph
import Debug.Trace

hexToBin :: Char -> [Int]
hexToBin = map digitToInt . showIntAtBase' 4 2 . digitToInt

solve1 :: String -> Int
solve1 input =
  sum [ sum . map (sum . hexToBin) . KnotHash.solve2 $ input ++ show row
      | row <- [0..127]
      ]

solve2 :: String -> Int
solve2 input = length . stronglyConnComp $ defragGraph
  where
    defragGraph =
      [ (u, u, neighbours u)
      | row <- [0..127]
      , (col, bit) <- binaryRow row
      , bit == 1
      , let u = (row, col)
      ]

    neighbours (row, col) =
      [              (row-1,col)
      , (row, col-1),           (row,col+1)
      ,              (row+1,col)
      ]

    binaryRow = zip [0..] . foldMap hexToBin . KnotHash.solve2 . (input ++) . show

main :: IO ()
main = do
  let test = "flqrgnkx-"
  let input = "jxqlasbh-"

  putStrLn ("TEST solve1: " ++ show (solve1 test == 8108))
  print . solve1 $ input

  putStrLn ("TEST solve2: " ++ show (solve2 test == 1242))
  print . solve2 $ input
