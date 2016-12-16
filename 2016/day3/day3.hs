import Data.List
import Data.List.Split

isTri [x,y,z] = x + y > z

part1
  = length
  . filter isTri
  . map ( sort
        . map read
        . words
        )

part2
  = length
  . filter isTri
  . map sort
  . concatMap (chunksOf 3)
  . transpose
  . map (map read . words)

main = do
  i <- lines <$> readFile "input.txt"
  print $ part1 i
  print $ part2 i
