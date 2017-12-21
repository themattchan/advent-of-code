#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

import Utils
import Data.List.Split
import qualified Data.Map as M

type Pattern = [String]

compileRules :: String -> (Pattern -> Pattern)
compileRules = (M.!) . M.unions . foldMap (perms . map (splitOn "/") . splitOn " => ") . lines
  where
    perms [pat, out] = map (flip M.singleton out) . (rotations <=< flips) $ pat
    perms _          = error "parse error"

flips :: Pattern -> [Pattern]
flips p = [p, reverse p, map reverse p]

rotations :: Pattern -> [Pattern]
rotations p = [ p
              , map reverse (transpose p) -- 90  CW
              , reverse (map reverse p)   -- 180 CW
              , transpose p               -- 270 CW
              ]

initialPattern :: Pattern
initialPattern = [".#." ,"..#" ,"###" ]

splitUp :: Pattern -> [[Pattern]]
splitUp ps = map (transpose . map (chunksOf n)) . chunksOf n $ ps
  where
    n | even (length ps) = 2
      | otherwise = 3

recombine :: [[Pattern]] -> Pattern
recombine = concatMap (map concat) . map transpose

solve :: String -> (Int, Int)
solve s = (count $ go !! 5, count $ go !! 18)
  where
    go     = iterate (recombine . fmap (fmap expand) . splitUp) initialPattern
    expand = compileRules s
    count  = length . filter (== '#') . concat

main :: IO ()
main = readFile "input.txt" >>= print . solve
