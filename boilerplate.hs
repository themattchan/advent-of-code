#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns, DeriveFunctor #-}
import Utils

solve :: String -> a
solve = undefined

main :: IO ()
main = readFile "input.txt" >>= print . solve
