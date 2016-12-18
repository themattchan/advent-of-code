import Data.List
import Data.Ord

solve f = map (head . f (comparing length) . group . sort) . transpose

part1 = solve maximumBy
part2 = solve minimumBy

main = do
  i <- lines <$> readFile "input.txt"
  print $ part1 i
  print $ part2 i
