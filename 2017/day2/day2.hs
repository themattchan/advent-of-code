#! /usr/bin/env runhaskell -i../../
import Utils

checksum1 :: [[Int]] -> Int
checksum1 = sum . map (uncurry (-) . (maximum &&& minimum))

-- is there a fancy number theoretic method for this
checksum2 :: [[Int]] -> Int
checksum2 = sum . map divisor
  where
    divisor xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]

main :: IO ()
main = do
  ss <- numbers <$> getContents
  print $ checksum1 ss
  print $ checksum2 ss
