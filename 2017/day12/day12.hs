#! /usr/bin/env runhaskell -i../../
import Utils
import Data.Graph

readGraph :: Parser [(Int,Int,[Int])]
readGraph = many $ do
  u <- num
  string " <-> "
  vs <- sepBy num (string ", ")
  spaces
  return (u,u,vs)

sizeOfScc :: SCC a -> Int
sizeOfScc = length . flattenSCC

findScc :: Int -> [SCC Int] -> Maybe (SCC Int)
findScc i sccs = headMay [ scc | scc <- sccs, i `elem` flattenSCC scc]

solve :: String -> (Int, Int)
solve ss = do
  graph <- runParser readGraph ss
  let sccs = stronglyConnComp graph
  scc0 <- findScc 0 sccs
  return (sizeOfScc scc0, length sccs)

main :: IO ()
main = readFile "input.txt" >>= print . solve
