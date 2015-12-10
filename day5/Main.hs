import Data.List
import Control.Monad

gtThreeVowels = (3 <=) . length . filter (flip elem "aeiou")

hasTwice w = length w > length (group w)

noInvalidStrings w = all (== False) $ map (flip isInfixOf w) bads
  where bads = ["ab","cd","pq","xy"]

isNice w = gtThreeVowels w && hasTwice w && noInvalidStrings w

main = do
  strs <- liftM lines (readFile "input.txt")
  print $ length (filter isNice strs)
