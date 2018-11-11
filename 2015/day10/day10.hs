import Data.List
import Control.Arrow
import Data.Semigroup

input = "3113322113"

doit :: Int -> String -> String
doit n = appEndo $ stimes n $ Endo f
  where f = foldMap (uncurry mappend . (show . length &&& (:[]) . head)) . group

main = do
  print $ length $ doit 40 input
  print $ length $ doit 50 input
