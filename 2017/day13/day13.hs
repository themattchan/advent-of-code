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
How to number the levels:

example: when depth = 3, the counter goes:

[x] -> [ ] -> [ ] -> [ ] -> [x] ....
[ ]    [x]    [ ]    [x]    [ ] ....
[ ]    [ ]    [x]    [ ]    [ ] ....

 0      1      2      3      4

this is Z_4. (0 == 4)

0,2 are self inverses.
1,3 are inverses.

example: when depth = 4, the counter goes:

[x] -> [ ] -> [ ] -> [ ] -> [ ] -> [ ] -> [x] ....
[ ]    [x]    [ ]    [ ]    [ ]    [x]    [ ] ....
[ ]    [ ]    [x]    [ ]    [x]    [ ]    [ ] ....
[ ]    [ ]    [ ]    [x]    [ ]    [ ]    [ ] ....

(0      1      2      3)     4      5      6

see that endpoints are only hit once, while the middle bits are
notice that 6 = 0, so this is Z_6 if you count the sequence.

0,3 are self units
2,4 are inverses
1,5 are inverses.

The counter is always in a group with two self inverses.

the depth is Z_n. counter is Z_{ 2n - 2 }.

in other words, the a scanner of depth n is characterised by the group

  S_d = Z_{2n-2} / phi
    where
      phi : Z_{2n-2} -> Z_n
      phi(e) = if e > n then e^-1 else e

it is obvious that the quotient group Z_{2n-2} is isomorphic to Z_n:
when e = 0, e^-1 = 0
when e = n-1, e^-1 = n-1 (where 2*(n-1) = 2n-2 = 0)
otherwise, e = x, e^-1 = 2n-2-x. (pairs)

furthermore, Z_n is a normal subgroup of Z_{2n-2}, because Z_{2n-2} is abelian
and all subgroups of abelian groups are normal.

NB: 1 generates all these groups.

Let:

- INPUT: let the input be { (0,d0), (1,d1) ...} where each pair is (level, depth).

- CONFIGURATION:
  define the CONFIGURATION of the system to be the an element of the
  subgroup of the direct product

   SYSTEM ::=  Z_{2*d0 -2} x Z_{2*d1 -2} x Z_{2*d2 -2}.....

  generated by

   GEN = (1,1,1,.....) \in SYSTEM

- CONFIGURATION STEP: A step (i.e. the evolution of the system by one time step)
  is defined as

   STEP(conf) = conf + GEN

- PATH. At each tick, the packet advances by a level. So it will reach level n
  by n ticks. Given a configuration CONFIG indexed by level, we can find the
  positions of each scanner by:

   PATH (conf) = { pos + (1 + 1 + ... #level of 1's ) | (level , pos) <- conf }


PART 1:

\conf -> SUM { level * pos | (level, pos) <- PATH(conf), phi(pos) == 0 }

PART 2:

use number theory.


-}
