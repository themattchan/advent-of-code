#! /usr/bin/env runhaskell -i../../
-- compile with
-- ghc day5.hs  -O2 -i../../

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
import Control.DeepSeq
import Control.Parallel.Strategies
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.List.Split
import GHC.Generics
import Utils
import Debug.Trace
newtype X = X (S.Seq Char) deriving (Generic, NFData)

xlen (X xs) = S.length xs

polar l r = (isUpper r && isLower l) || (isUpper l && isLower r)
cmp = (==) `on` toLower
---isChar c x y = toLower x == c && toLower y == c

toX = X . S.singleton
unX (X xs) = toList xs

-- copied from 2015 day 4 --- there should be a better way to do this...
pmconcat :: (NFData a, Monoid a) => Int -> [a] -> a
pmconcat cs = go where
  go [x] = x
  go xs  = go . withStrategy (parList rdeepseq) . map mconcat $ chunksOf cs xs


instance Monoid X where
  mempty = X mempty
  mappend (X (ls1 S.:|> r)) (X (l S.:<| rs2))
    | polar l r && cmp l r
    = X ls1 <> X rs2
  mappend (X xs) (X ys) = X (xs <> ys)

solvePar :: String -> Int
solvePar = length . loop --- . pmconcat 250 . map toX
  where
    go = force . unX . pmconcat 500 . map toX -- . trace "go"
    loop x = let x' = go x in if x == x' then x' else go x'  -- this is gross

-- mymconcat :: (Monoid a, Foldable t) => (a -> a -> a) -> t a -> a
-- mymconcat (<>) = foldr (<>) mempty

-- part2mappend :: Char -> S.Seq Char -> S.Seq Char -> S.Seq Char
-- part2mappend c (ls1 S.:|> r) (l S.:<| rs2)
--     | polar l r && isChar c l r
--     = part2mappend c ls1 rs2
-- part2mappend c xs ys = xs <> ys

-- pmconcatWith :: (NFData a, Monoid a) => (a->a->a) -> Int -> [a] -> a
-- pmconcatWith (<>) cs = go where
--   go [x] = x
--   go xs  = go . withStrategy (parList rdeepseq) . map (mymconcat (<>)) $ chunksOf cs xs

-- solvePar2 :: Char -> String -> Int
-- solvePar2 c = length . loop
--   where
--     go = force . toList . pmconcatWith (part2mappend c) 500 . map S.singleton
--     loop x = let x' = go x in if x == x' then x' else go x'

doPart2 :: String -> Int
doPart2 s = minimum $ parMap rdeepseq (\c -> solvePar $ filter ((/= c) . toLower) s) ['a'..'z']

main = print . (solvePar &&& doPart2) . filter (not . isSpace) =<< readFile "input.txt"
--main = test
test = print $ doPart2 "dabAcCaCBAcCcaDA"
