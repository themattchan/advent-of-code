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
data Scanner = Scanner {-#UNPACK#-}!Int {-#UNPACK#-}!Int !Dir deriving Show

stepScanner :: Scanner -> Scanner
stepScanner (Scanner i d UP)
  = Scanner (i+1) d (if (i+1) == d-1 then DOWN else UP)
stepScanner (Scanner i d DOWN)
  = Scanner (i-1) d (if (i-1) == 0 then UP else DOWN)

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
        go (l, Just (Scanner 0 d _)) = l*d
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
        go (_, Just (Scanner 0 _ _)) = True
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
      | n == l    = ((l, Just (Scanner 0 d UP)) :)
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

--------------------------------------------------------------------------------

{-
-- there is an analytic solution to part 2!!!
-- some number theoretic thing to do with common factors...
-- level : depth


Group: for depth n, define
G_n = {0..2n-1}. this is the same as Z_2n-1 with the additional constraint that
elements be interpreted in congruence class defined by (e - n).

in other words, the a scanner of depth d is characterised by the group

  S_d = Z_{2n-1} / phi
    where
      phi : Z_{2n-1} -> Z_n
      phi(e) = (e - n)_n.


levels: the number of "shifts" to 0 we need to add.

eg. if level = 4 and depth = 5, then this scanner starts at 4.

the entire state of the system { (0,d0), (1,d1) ...}
where each pair is (level, depth) is given by:

INIT = { 0 in S_d0, (0 + 1) in S_d1, (0+1+1) in S_d2 ... }

we take this sequence to be an element of the direct product:

S_DAll = S_d0 x S_d1 x S_d2 ....

find the number of 1's (in S_DAll) to add to INIT such that
all of the components are not in the congruence class of which
(0 in Z_2n-1, for n of the respective component) is a representative.


-}
