#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TypeApplications #-}
import qualified Text.Regex.TDFA as RE
import qualified Text.Regex.TDFA.String as RE
import qualified Data.Graph as G
import Utils
import Debug.Trace



--t1 :: [String] -> String
t1 is = map (\x -> fst $ fromJust $ find ((== x) . snd) vs)
        (G.topSort graph)
--        trace ("ggg: "<> show vs) $
--          (reverse (G.topSort (G.transposeG graph)))
  where
    graph = G.buildG (1,length vs) es'
    es = foldMap re1 is
    es' = [(i,j) | (x,y) <- es, let Just i = lookup x vs, let Just j = lookup y vs]
    vs = (`zip` [1..]) $ sort $ nub $ concat [[x,y] | (x,y) <- es ]

    re1 = getIt . RE.matchOnceText @RE.Regex @String rgx

    rgx :: RE.Regex
    rgx = RE.makeRegex @RE.Regex @_ @_ @String
            "^Step ([A-Z]) must be finished before step ([A-Z]) can begin\\.$"

    getIt (Just (_,ss,_)) = edge $ foldMap ((:[]) . fst) ss
    getIt Nothing = error "regex failed"

    edge [_,[x],[y]] = [(x, y)]
    edge x = error ("edge: " <> show x)

main = putStrLn . t1 . lines =<< readFile "test"
