#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns, DeriveFunctor #-}
import Utils
import Debug.Trace
import Data.Semigroup (stimes)

type FirewallState = [(Int, Maybe Scanner)]
type PacketPath    = [(Int, Maybe Scanner)]
  -- same thing, it's a major diagonal in a
  -- matrix where rows are time-stepped FirewallState's

data Dir = UP | DOWN deriving Show
newtype Scanner = Scanner (Int, Int, Dir)  deriving Show

stepScanner :: Scanner -> Scanner
stepScanner (Scanner (i, d, UP))
  = Scanner (i+1, d, if (i+1) == d-1 then DOWN else UP)
stepScanner (Scanner (i, d, DOWN))
  = Scanner (i-1, d, if (i-1) == 0 then UP else DOWN)

stepScannerMany :: Int -> Scanner -> Scanner
stepScannerMany n = appEndo (stimes n (Endo stepScanner))

stepFirewall :: FirewallState -> FirewallState
stepFirewall = fmap (fmap (fmap stepScanner))

stepPacketPath :: PacketPath -> PacketPath
stepPacketPath = stepFirewall

packetPathFromFirewallState :: FirewallState -> PacketPath
packetPathFromFirewallState st =
  [ (i, stepScannerMany i <$> scanner) | (i, scanner) <- st ]

solve1 :: String -> Int
solve1 = countCaught . packetPathFromFirewallState . generate
  where
    countCaught = sum . map go
      where
        go (l, Just (Scanner (0,d,_))) = l*d
        go _ = 0

solve2 :: String -> Int
solve2 = fst
       . head
       . dropWhile (unsafe . snd)
       . zip [0..]
       . iterate stepPacketPath
       . packetPathFromFirewallState
       . generate
  where
    unsafe = or . map go
      where
        go (_, Just (Scanner (0,_,_))) = True
        go _ = False

generate :: String -> FirewallState
generate = initScanner 0
         . map parseScannerDepth
         . lines
  where
    parseScannerDepth xs
      | [l,d] <- words xs = (read (init l), read d)
      | otherwise         = error "BOOM"

    initScanner n [] = []
    initScanner n ((l,d):xs)
      | n == l    = ((l, Just (Scanner (0, (d, UP)))) :)
                    $ initScanner (n+1) xs
      | otherwise = ((n, Nothing) :)
                    $  initScanner (n+1) ((l,d):xs)

main :: IO ()
main = do
  -- (24, 10)
  test <- readFile "test.txt"
  print . solve1 $ test
  print . solve2 $ test

  input <- readFile "input.txt"
  print . solve1 $ input
  print . solve2 $ input
