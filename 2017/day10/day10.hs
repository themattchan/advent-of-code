#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE LambdaCase, BangPatterns #-}
import Utils
import Data.Bits (xor)
import Data.List.Split (chunksOf)
import Debug.Trace
import Data.Word
import Control.Exception.Base

type State = ([Int], Int, Int, Int)
  -- ^ list being hashed, length of list, actual index of head, skip size

initState :: [Int] -> State
initState xs = (xs,length xs,0,0)

-- rotate elements such that head of l is at position d
finish :: State -> [Int]
finish (l,n,d,_) = rrot n d l

lrot :: Int -> Int -> [a] -> [a]
lrot n i = take n . drop i . cycle

rrot :: Int -> Int -> [a] -> [a]
rrot n i = lrot n (n-i)

-- encode one round
hashRound :: State -> [Int] -> State
hashRound = foldl' go
  where
    -- head of the list is always the current position
    -- d tracks the current position as an index in the original ordering
    go (l,n,d,s) x
      = let (l1,l2) = splitAt x l
            l' = lrot n (x+s) $ reverse l1 ++ l2
            d' = (d + x + s) `mod` n
        in (l', n, d', s+1)

sparseHash :: State -> [Int] -> State
sparseHash st = foldl' hashRound st . replicate 64

denseHash :: State -> String
denseHash = foldMap (showHex' . foldr1 xor) . chunksOf 16 . finish

encodedAscii :: String -> [Int]
encodedAscii s = map ord (trim s) ++ [17, 31, 73, 47, 23]
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace

solve1 :: [Int] -> [Int] -> Int
solve1 xs = product . take 2 . finish . hashRound (initState xs)

solve2 :: String -> String
solve2 = (\s -> assert (length s == 32) s)
       . denseHash . sparseHash (initState [0..255])
       . encodedAscii

test = flip assert "TESTS PASSED"
     $ and [ solve1 [0..4] [3,4,1,5] == 12
           , solve2 ""
             == "a2582a3a0e66e6e86e3812dcb672a272"
           , solve2 "AoC 2017"
             == "33efeb34ea91902bb2f59c9920caa6cd"
           , solve2 "1,2,3"
             == "3efbe78a8d82f29979031a4aa0b16a9d"
           , solve2 "1,2,4"
             == "63960835bcdc130f0b66d7ff4f6a5a8e"
           ]

main :: IO ()
main = do
  putStrLn test

  input <- readFile "input.txt"
  let lengths = read $ "[" ++ input ++ "]"
  print $ solve1 [0..255] lengths
  putStrLn $ solve2 input
