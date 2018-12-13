import Utils
import Data.List.Split (splitOn)
import qualified Data.Map as M

main = do
  (initSt0:_, rules0) <- splitAt 2 . lines <$> readFile "input.txt"
  let Just initSt = stripPrefix "initial state: " initSt0
  let rules = M.fromList $ map go rules0
        where
          go x = let [pat, [replace]] = splitOn " => " x in (pat,replace)
-- FIXME: '.' extends infinitely to left and right!!
  let step = map (rules M.!) . window 5
  let system = iterate step initSt
--  zipWith [-2..] $ system !! 20
  print $ take 21 system
