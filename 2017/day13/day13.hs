#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns, DeriveFunctor #-}
import Utils
import Debug.Trace

data Dir = UP | DOWN deriving Show

newtype Scanner = Scanner (Int, Int, Dir)  deriving Show
type FirewallState = [(Int, Maybe Scanner)]
type PacketPath = [(Int, Maybe Scanner)]
                -- same thing, it's a diagonal in a
                -- matrix where rows are FirewallState's

stepScanner :: Scanner -> Scanner
stepScanner (Scanner (i, d, UP))   = Scanner (i+1, d, if (i+1) == d-1 then DOWN else UP)
stepScanner (Scanner (i, d, DOWN)) = Scanner (i-1, d, if (i-1) == 0 then UP else DOWN)

stepFirewall = fmap (fmap (fmap stepScanner))

solve1 :: String -> Int
solve1 = part1 . generate
  where
    part1 (n, st) = countCaught $ zipWith (!!) st [0..n]

countCaught :: PacketPath -> Int
countCaught = sum . map go
  where
    go (l, Just (Scanner (0,d,_))) = l*d
    go _ = 0

isSafe :: PacketPath -> Bool
isSafe = and . map go
  where
    go (_, Just (Scanner (0,_,_))) = False
    go _ = True

solve2 :: String -> Int
solve2 = part2 . generate
  where
    part2 (n,st0) = fst . head . dropWhile (not . snd) $ unfoldr go (0, st0)
      where
        go (i, st) = Just ((i,countThisOffset st), (i+1, tail st))

        countThisOffset = isSafe . flip (zipWith (!!)) [0..n]

generate :: String -> (Int, [FirewallState])
generate = fmap (iterate stepFirewall)
         . initScanner 0
         . map parseScannerDepth
         . lines
  where
    parseScannerDepth xs
      | [l,d] <- words xs = (read (init l), read d)
      | otherwise         = error "BOOM"

    -- returns max index as well as a filled list
    initScanner n [] = (n-1,[])
    initScanner n ((l,d):xs)
      | n == l    = ((l, Just (Scanner (0,d, UP))) :)
                    <$> initScanner (n+1) xs
      | otherwise = ((n, Nothing) :)
                    <$> initScanner (n+1) ((l,d):xs)

main :: IO ()
main = do
  -- (24, 10)
  test <- readFile "test.txt"
  print . solve1 $ test
  print . solve2 $ test

  input <- readFile "input.txt"
  print . solve1 $ input
  print . solve2 $ input
