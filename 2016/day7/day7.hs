#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import Data.List.Split
import Debug.Trace

solve :: [String] -> (Int, Int)
solve = (part1 &&& part2) . map ((evens &&& odds) . splitOneOf "[]")
  where
    part1 = length . filter findTLS
    findTLS = uncurry (&&) . (findABBA *** not . findABBA)
    findABBA = or . map (any (\[w,x,y,z] -> w == z && x == y && w /= x) . window 4)

    part2 = length . filter findSSL
    findSSL = not . null . uncurry (intersectBy cmp) . (foldMap findABA *** foldMap findABA)
    findABA = filter (\[x,y,z] -> x == z && x /= y) . window 3
    cmp x [b,a,_] = x == [a,b,a]

    evens = select [pure, const []]
    odds  = select [const [], pure]
    select = concat ... zipWith ($) . cycle

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines
