#! /usr/bin/env runhaskell -i../../
import Utils

main :: IO ()
main = do
  print $ solve1 input
  print $ solve2 (fromIntegral input)

input :: Int
input = 312051

--------------------------------------------------------------------------------

solve1 :: Int -> Int
solve1 input =
  let
    -- lower part of major diagonal is squares of odd numbers:
    -- 1,9,25,49,...
    -- square root = side length of inner square.

    oddsqs = map (id &&& (^2)) [1,3..]
    (ring, anchor) = head . dropWhile ((< input) . snd) $ oddsqs

    -- find the side of the square the number is on:
    -- normalise it to a centre line
    -- this is also the distance from 1 -> ring.
    anchor2centre = ring `div` 2
    centres = [ anchor - (anchor2centre * m) | m <- [1,3,5,7] ]

    dist2centre = minimum [ abs (input - c) | c <- centres ]

  in anchor2centre + dist2centre

--------------------------------------------------------------------------------

ringsize :: Int -> Int
ringsize 0 = 1
ringsize n = 8 * n

nthOdd :: Integral a => a -> a
nthOdd n = 2*n + 1

sides :: Int -> [[Int]]
sides n = take 4 $ unfoldr go indexes
  where
    go      = Just . (take slen &&& drop (slen-1))
    indexes = drop (rs-1) $ cycle (take rs [0..])
    slen    = nthOdd n
    rs      = ringsize n

-- indices of corners
corners :: Int -> [Int]
corners = map head . sides

window :: Int -> [a] -> [[a]]
window n xs = go (length xs - n + 1) xs
  where
    go 0 _  = []
    go w ys = take n ys : go (w-1) (tail ys)

-- (indicies of) neighbours of (indicies of) this ring in previous ring.
prevNeighbours :: Int -> [(Int, [Int])]
prevNeighbours n = concatMap (tail . go) ns
  where
    go (ps,ts) = map (fmap catMaybes) $ zip ts ws
      where
        ws = window 3 $ [Nothing, Nothing] ++ map Just ps ++ [Nothing, Nothing]

    ns = zip (sides (n-1)) (sides n)

seq2 :: [Integer]
seq2 = concat $ go 0 [1]
  where
    go n prevRing = prevRing : go n' nextRing
      where
        n' = n+1
        nextRing = [   (if i > 0 then nextRing !! (i-1) else 0)
                     -- numbers after a corner are neighbours with the corner
                     -- (above) and the number before the corner diagonally
                     + (if i `elem` afterCorners then nextRing !! (i-2) else 0)
                     -- the last two numbers are neighbours with number 0 in this ring
                     + (if i >= ringsize n' - 2 then nextRing !! 0 else 0)
                     + pns
                   | (i, pns) <- prevNeighbourSums
                   ]
        afterCorners = tail $ map (+1) (corners n')
        prevNeighbourSums = map (fmap (sum . map (prevRing !!))) $ prevNeighbours n'

solve2 :: Integer -> Integer
solve2 input = head $ dropWhile (<= input) seq2
