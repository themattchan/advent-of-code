#! /usr/bin/env runhaskell -i../../

{-# LANGUAGE
    TupleSections
  , BangPatterns
  , ScopedTypeVariables
#-}

-- Compile with:
--    stack ghc -- day5.hs -O2 -i ../../

import Utils
import Zipper
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Int

solve :: (Int -> Int) -> Zipper Int -> Int
solve f = foldl' (+) 1 . unfoldr go
  where
    go = fmap (1, ) . uncurry move . (viewFoci &&& modifyFoci f)

part2Func :: Integral a => a -> a
part2Func x | x >= 3    = x-1
            | otherwise = x+1

main1 :: IO ()
main1 = do
  zipper <- zipperFromList . map read . lines <$> readFile "input.txt"
  timed $ print $ solve (+1) zipper
  -- this works if you compile the program
  -- but blows the stack in ghci
  timed $ print $ solve part2Func zipper

--------------------------------------------------------------------------------

solve' :: [Int64] -> (Int64 -> Int64) -> IO Int
solve' input f = do
  let !size = length input
  vector :: V.IOVector Int64 <- V.unsafeNew size
  forM_ (zip [0..] input) $ uncurry (V.unsafeWrite vector)
  go 0 0 size vector
  where
    go !steps !idx !end vec
      | idx >= end = return steps
      | otherwise = do
          s <- V.unsafeRead vec idx
          V.unsafeWrite vec idx (f s)
          go (steps +1) (idx + (fromIntegral s)) end vec

main2 :: IO ()
main2 = do
  input <- map read . lines <$> readFile "input.txt"
  timed $ print =<< solve' input (+1)
  timed $ print =<< solve' input part2Func

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "build/fold"
  timed main1

  putStrLn ""

  putStrLn "IO MVector"
  timed main2

{-
build/fold
376976
Computation time: 1.978 sec
29227751
Computation time: 15.504 sec
Computation time: 17.483 sec

IO MVector
376976
Computation time: 0.524 sec
29227751
Computation time: 49.625 sec
Computation time: 50.150 sec

------------------------------------------------

with -O2

build/fold
376976
Computation time: 0.561 sec
29227751
Computation time: 1.673 sec
Computation time: 2.235 sec

IO MVector
376976
Computation time: 0.091 sec
29227751
Computation time: 6.748 sec
Computation time: 6.840 sec

------------------------------------------------

foldr and foldl' are roughly the same with or without optimisation


------------------------------------------------

UPDATE:

IORefs are really slow!

(no optimisation, no ioref)
IO MVector
376976
Computation time: 0.159 sec
29227751
Computation time: 15.125 sec
Computation time: 15.284 sec
-}
