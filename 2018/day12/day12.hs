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
  let extend = ("....." <>) . (<> ".....") -- subtract 3 each iteration
  let leftmostIdx n = (- 3) * n
  let step = map (rules M.!) . window 5 . extend
  let system = iterate step initSt
  let computeFor n = sum [ i |(i,c) <- zip [leftmostIdx n ..] (system !! n), c == '#' ]
  print $ computeFor 20
--  print $ computeFor 50000000000
