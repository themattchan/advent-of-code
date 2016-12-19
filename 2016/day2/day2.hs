{-# LANGUAGE PatternGuards #-}
import Control.Arrow ((***), (&&&))
import Data.Bifunctor
import Data.List
import Text.Printf

move1 k 'U' | k > 3          = k-3
move1 k 'D' | k < 7          = k+3
move1 k 'L' | k `mod` 3 /= 1 = k-1
move1 k 'R' | k `mod` 3 /= 0 = k+1
move1 k _ = k

-- 1985
test = "ULL\nRRDDD\nLURDL\nUUUUD"

maincol = [1,3,7,11,13]

move2 k 'U'
  | k `elem` [5,2,1,4,9] = k
  | even k               = k - 4
  | Just i <- k `elemIndex` maincol
  = maincol !! max 0 (i-1)
move2 k 'D'
  | k `elem` [5,10,13,12,9]  = k
  | even k                   = k + 4
  | Just i <- k `elemIndex` maincol
  = maincol !! min 4 (i+1)
move2 k 'L'
  | k `notElem` [1,2,5,10,13] = k - 1
move2 k 'R'
  | k `notElem` [1,4,9,12,13] = k + 1
move2 k _ = k

hex :: Int -> String
hex = printf "%x"

main = go <$> file >>= print . bimap n n
  where
    file = readFile "input.txt"
    go = unzip . tail . scanl (foldl ((uncurry (&&&)) . (move1 *** move2))) (5,5) . lines
    n = foldMap hex
