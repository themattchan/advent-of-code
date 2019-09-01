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
    fwdG = M.fromListWith (<>) [(i, S.singleton j) | (i,j) <- es]
    bwdG = M.fromListWith (<>) [(j, S.singleton i) | (i,j) <- es]

    es = foldMap re1 is

    re1 = foldMap (\f -> [(f 1, f 2)]) . match rgx

    rgx :: Regex
    rgx = makeRegex "^Step ([A-Z]) must be finished before step ([A-Z]) can begin\\.$"

mySort :: (MyGraph, MyGraph) -> String
mySort (fg, bg) =
  trace ("\n|ret|="<>show (S.size $ S.fromList ret)
          <> "  |vs|="<> show (S.size vs)
          <> "  |ks|="<>show (S.size ks)
          <> "  |allNodes|="<>show(S.size allNodes)
          <> "\nkeys="<>show (fold$S.toAscList ks)
          <> "\nret="<> sort ret
          <> "\ndiff=" <> show (allNodes S.\\ S.fromList (map (:[])ret))
          <> "\nf="<>show f
          <> "\nnoDups="<> show(S.size (S.fromList ret) == length ret)
        )
    ret
  where
    ret = go [f] (S.singleton f) S.empty (S.toAscList $ fg M.! f)

    f = S.findMin (ks S.\\ vs)

    allNodes = ks<>vs
    ks = M.keysSet fg
    vs = fold fg

    splitPending seen pending =
      -- returns (still pending, avail)
      S.partition (\n -> fold (M.lookup n bg) `S.intersection` pending /= S.empty)
      $ S.filter (\n -> n `S.notMember` seen)
      $ pending

    go ret retS pending []
      | S.null pending = concat $ (reverse ret) <> S.toAscList (allNodes S.\\ retS)
      | otherwise =
        let (pending', todo) = --traceShowId  $
              splitPending retS pending
        in -- trace (" retS = "<> show retS) $
          go ret retS pending' (S.toAscList todo)

    go ret retS pending (t:todo) =
      trace (
      "\ngo: visit "<> t
      --  <> " retS'="<> show retS'
        <> " \npending="<>show pending
        <>"\npending'="<>show pending'
        <> "   new="<>show new
        <> "\ntodo="<>show todo
        <> "   todo'=" <>show todo'
      ) $
        go (t:ret) retS' pending' todo'
      where
        retS' = S.insert t retS

        (pending', new) = splitPending retS' (pending <> (fold $ M.lookup t fg))

        todo' = S.toAscList (new <> S.fromList todo)

main :: IO ()
main = print . mySort . buildGraph . lines =<< readFile "input"
