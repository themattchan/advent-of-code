import Control.Applicative
import Data.Profunctor
import Data.List
import Data.List.Split

isTri [x,y,z] = x + y > z

process = dimap (map (map read . words))
                (length . filter isTri . map sort)

part1 = process id
part2 = process (concatMap (chunksOf 3) . transpose)

main = do
  i <- lines <$> readFile "input.txt"
  print $ part1 i
  print $ part2 i
