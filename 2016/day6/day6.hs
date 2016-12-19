import Control.Arrow
import Data.Bifunctor
import Data.List
import Data.Ord

solve = bimap h h . unzip . map (f (comparing length) . group . sort) . transpose
  where
    f = uncurry (&&&) . (maximumBy &&& minimumBy)
    h = map head

main = do
  lines <$> readFile "input.txt" >>= print . solve
