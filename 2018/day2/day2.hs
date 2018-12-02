import Control.Arrow
import Data.List

solve1 :: [String] -> Int
solve1 = (*) <$> s2 <*> s3
  where
    f n = not . null . filter (==n) . map length . group . sort
    s2 = length . filter (f 2)
    s3 = length . filter (f 3)

solve2 :: [String] -> Maybe String
solve2 = fmap (map fst . snd) . find ((==1) . length . fst) . map diff . (zip <*> tail) . sort
  where
    diff = (filter (uncurry (/=)) &&& filter (uncurry (==))) . uncurry zip

main :: IO ()
main = print . (solve1 &&& solve2) . lines =<< readFile "input.txt"
