#! /usr/bin/env runhaskell -i../../
-- compile with
-- ghc day5.hs  -O2 -i../../

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.List.Split
import Utils
import Debug.Trace

newtype X = X (S.Seq Char)

xlen (X xs) = S.length xs

polar l r = (isUpper r && isLower l) || (isUpper l && isLower r)
cmp = (==) `on` toLower

toX = X . S.singleton
unX (X xs) = toList xs

instance Monoid X where
  mempty = X mempty
  mappend (X (ls1 S.:|> r)) (X (l S.:<| rs2))
    | polar l r && cmp l r
    = X ls1 <> X rs2
  mappend (X xs) (X ys) = X (xs <> ys)

solve :: String -> Int
solve = length . loop
  where
    go = unX . foldMap toX
    loop x = let x' = go x in if x == x' then x' else go x'  -- this is gross

doPart2 :: String -> Int
doPart2 s = minimum $ map (\c -> solve $ filter ((/= c) . toLower) s) ['a'..'z']

main = print . (solve &&& doPart2) . filter (not . isSpace) =<< readFile "input.txt"

test = print $ doPart2 "dabAcCaCBAcCcaDA"
