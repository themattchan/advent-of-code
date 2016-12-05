import Data.List
import Control.Monad

gtThreeVowels = (3 <=) . length . filter (flip elem "aeiou")

hasTwice w = length w > length (group w)

noInvalidStrings w = all (== False) $ map (flip isInfixOf w) bads
  where bads = ["ab","cd","pq","xy"]

isNice w = gtThreeVowels w && hasTwice w && noInvalidStrings w

pairNoOverlap w = length rmOverlaps > length (nub rmOverlaps)
  where rmOverlaps = concat . filter ((< 2) . length) $ group (zip w (tail w))

window n xs = if n > length xs then [] else take n xs : window n (tail xs)

letterRepeat = any repsep . window 3
  where repsep [a,b,c] = a==c && b /= a

isNice2 w = pairNoOverlap w && letterRepeat w

main = do
  strs <- liftM lines (readFile "input.txt")
  print $ length (filter isNice strs)
  print $ length (filter isNice2 strs)
