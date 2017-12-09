#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE LambdaCase, BangPatterns #-}
import Utils

solve :: String -> (Int, Int)
solve = go 0 0 0 . unbang
  where
    go !l !s !g = \case
      []     -> (s,g)
      '{':xs -> go (l+1) s g xs
      '}':xs -> go (l-1) (s+l) g xs
      '<':xs -> let (gs,_:rest) = span (/='>') xs
                in go l s (g+length gs) rest
      _ : xs -> go l s g xs

    unbang []             = []
    unbang ('!' : _ : xs) = unbang xs
    unbang (x:xs)         = x : unbang xs

main :: IO ()
main = readFile "input.txt" >>= timed . print . solve
