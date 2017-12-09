#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE DeriveFunctor, PatternGuards #-}

import Utils

-- solve1 :: String -> Sum Int
-- solve1 = Sum . uncurry ((-) `on` length) . (id &&& r)
--   where r = (read :: String -> String)

solve1 :: String -> Int
solve1 = go 2 . init . tail
  where
    go n ('\\' : 'x' : _ : _ : xs ) = go (n+3) xs
    go n ('\\' : '\\' : xs) = go (n+1) xs
    go n ('\\' : '"' : xs) = go (n+1) xs
    go n (_:xs) = go n xs
    go n _ = n

solve2 :: String -> Int
solve2 = (+2) . length . filter (`elem` "\"\\")

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  print $ foldMap (Sum . solve1 &&& Sum . solve2) ls
