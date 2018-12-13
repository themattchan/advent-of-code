#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TypeApplications #-}
import qualified Data.Graph as G
import Utils
import Debug.Trace



--t1 :: [String] -> String
t1 is = trace ("vs: " <> show vs) $ map (\x -> fst $ fromJust $ find ((== x) . snd) vs)
         (myTopSort vs es' graph)
--        trace ("ggg: "<> show vs) $
--          (reverse (G.topSort (G.transposeG graph)))
  where
    graph = G.buildG (1,length vs) es'
    es = foldMap re1 is
    es' = [(i,j) | (x,y) <- es, let Just i = lookup x vs, let Just j = lookup y vs]
    vs = (`zip` [1..]) $ sort $ nub $ concat [[x,y] | (x,y) <- es ]

    re1 = getIt . match rgx

    rgx :: Regex
    rgx = makeRegex "^Step ([A-Z]) must be finished before step ([A-Z]) can begin\\.$"

    getIt (Just (_,ss,_)) = edge $ foldMap ((:[]) . fst) ss
    getIt Nothing = error "regex failed"

    edge [_,[x],[y]] = trace ([x]<> " " <>[y]) [(y,x)] -- y depends on x
    edge x = error ("edge: " <> show x)

-- g is a "depends on" graph

--myTopSort :: [G.Edge] -> G.Graph -> [G.Vertex]
myTopSort vs es g = go [] (G.vertices g)
  where
    vOuts v = undefined
--    vOuts done v = sort [j | (i,j) <- es, i==v, j `notElem` done]
    go out [] = out
    go out (v:todo)
--      | trace ("DO: "<> show v <> show (lookup v (map swap vs))) False = undefined
      | null (vOuts out v) = go (v:out) todo
      | otherwise = let doit = (\x -> trace ("doit " <> show x) x) $ vOuts out v
                        todo' = todo \\ doit
                    in go out (doit ++ (v:todo'))



main = putStrLn . t1 . lines =<< readFile "test"
