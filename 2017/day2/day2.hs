#! /usr/bin/env runhaskell -i../../
import Utils

checksum1 :: [[Int]] -> Int
checksum1 = sum . map (uncurry (-) . (maximum &&& minimum))

main :: IO ()
main = do
  ss <- map (map read . words) . lines <$> getContents
  print $ checksum1 ss
