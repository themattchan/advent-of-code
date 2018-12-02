import Control.Arrow
import Data.List

f n = not . null . filter (==n) . map length . group . sort
s2 = length . filter (f 2)
s3 = length . filter (f 3)
solve1 = uncurry (*) . (s2 &&& s3)
main = print . solve1 . lines =<< readFile "input.txt"
