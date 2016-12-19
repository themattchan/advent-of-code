import Control.Arrow
import Data.Bifunctor
import Data.List
import Data.Ord

solve = (bimap `w` map head) . unzip . map (f (comparing length) . group . sort) . transpose
  where f = uncurry (&&&) . (maximumBy &&& minimumBy)
        w f x = f x x

main = lines <$> readFile "input.txt" >>= print . solve
