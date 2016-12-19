import Control.Arrow ((***), (&&&))
import Data.Bifunctor
import Data.List

move k 'U' | k > 3          = k-3
move k 'D' | k < 7          = k+3
move k 'L' | k `mod` 3 /= 1 = k-1
move k 'R' | k `mod` 3 /= 0 = k+1
move k _ = k

-- 1985
test = "ULL\nRRDDD\nLURDL\nUUUUD"

move2 k 'U' | k > 3          = k-3
move2 k 'D' | k < 7          = k+3
move2 k 'L' | k `mod` 3 /= 1 = k-1
move2 k 'R' | k `mod` 3 /= 0 = k+1
move2 k _ = k

main = go <$> file >>= print . bimap n n
  where
    file = readFile "input.txt"
    go = unzip . tail . scanl (foldl ((uncurry (&&&)) . (move *** move2))) (5,5) . lines
    n = foldMap show
