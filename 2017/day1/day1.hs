#! /usr/bin/env runhaskell -i../../
import Utils

solve :: Int -> [Int] -> Int
solve n = getSum
        . foldMap (Sum . fst)
        . filter (uncurry (==))
        . uncurry zip
        . (id &&& drop n . cycle)

main :: IO ()
main = do
  ints <- map digitToInt . filter isDigit <$> getContents
  print $ solve 1 ints
  print $ solve (length ints `div` 2) ints
