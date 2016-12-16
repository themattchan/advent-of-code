import Data.List

move k 'U' | k > 3          = k-3
move k 'D' | k < 7          = k+3
move k 'L' | k `mod` 3 /= 1 = k-1
move k 'R' | k `mod` 3 /= 0 = k+1
move k _ = k

-- 1985
test = "ULL\nRRDDD\nLURDL\nUUUUD"

main = go <$> file >>= putStrLn . foldMap show
  where
    file = readFile "input.txt"
    go = tail . scanl (foldl move) 5 . lines
