#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TypeApplications #-}
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import Utils
import Debug.Trace

type MyGraph = M.Map String (S.Set String)

buildGraph :: [String] -> (MyGraph, MyGraph)
buildGraph is = (fwdG, bwdG)
  where
    fwdG = M.fromListWith (<>) fes'
    bwdG = M.fromListWith (<>) bes'

    es = foldMap re1 is

    fes' = [(i, S.singleton j) | (i,j) <- es]
    bes' = [(j, S.singleton i) | (i,j) <- es]

    re1 = foldMap (\f -> [(f 1, f 2)]) . match rgx

    rgx :: Regex
    rgx = makeRegex "^Step ([A-Z]) must be finished before step ([A-Z]) can begin\\.$"

mySort :: (MyGraph, MyGraph) -> String
mySort (fg, bg) = trace ("|ret|="<>show (length ret)<>"  |keys|="<> show (S.size vs))
    ret
  where
    ret = go ([f], S.empty) (S.toAscList $ fg M.! f)

    f = S.findMin (ks S.\\ vs)

    ks = M.keysSet fg
    vs = fold fg

    go (ret, _) [] = concat $ reverse ret
    go (ret, retS) (t:todo) = go (t:ret, retS') todo'
      where
        retS' = S.insert t retS
        new = S.filter (\n -> fold (M.lookup n bg) `S.isSubsetOf` retS') (fold $ M.lookup t fg)
        todo' = S.toAscList (new <> S.fromList todo)


main = print . mySort . buildGraph . lines =<< readFile "input"
