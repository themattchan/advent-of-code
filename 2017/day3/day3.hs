#! /usr/bin/env runhaskell -i../../
import Utils
import Debug.Trace

input :: Int
input = 312051

solve1 input =
  let
    -- lower part of major diagonal is squares of odd numbers:
    -- 1,9,25,49,...
    -- square root = side length of inner square.

    oddsqs = map (id &&& (^2)) [1,3..]
    (ring, anchor) = head . dropWhile ((< input) . snd) $ oddsqs

    -- distance from anchor to 1: anchor -1

    -- find the side of the square the number is on:
    -- normalise it to a centre line
    dist2centre = ring `div` 2
    centres = [ anchor - (dist2centre * m) | m <- [1,3,5,7] ]

    mindist = minimum [ abs (input - c) | c <- centres ]

  in dist2centre + mindist
