#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import Data.Graph
import Data.Tree
import Data.List.Split
import qualified Data.Set as S
import qualified Data.IntMap as M
import Debug.Trace

--data Pair a = P a a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

readInput :: String -> [(Int,Int)]
readInput = map (toPair . splitOn "/") . lines
  where
    toPair [x,y] = (,) (read x) (read y)

gen pairs = go 0 0 0 pairs where
  go !match !size !len avail =
    (len,size) : do (x,y) <- avail
                    match' <- if x == match then [y] else if y == match then [x] else []
                    go match' (size + x + y) (len+1) (delete (x,y) avail)

solve1 = maximum . map snd
solve2 = maximum . snd . M.findMax . M.fromList . map (fmap (:[]))

-- type Path a = [a]

-- paths :: Tree a -> [[a]]
-- paths (Node n []) = [[n]]
-- paths (Node n kids) = concat [ map (n:) (paths k) | k <- kids]


-- -- this thing is fucked
-- -- allPaths :: Graph -> [Vertex] -> [Path Vertex]
-- -- allPaths = concatMap paths ... dfs

-- isStart (P a b) = a == 0 || b == 0

-- isNeighbour l@(P a b) (P x y)
-- --  | isStart l = (max a b) == x || (max a b) == y
--   | otherwise = a == x || b == x || a == y || b== y

-- --solve1 :: [Pair Int] -> Int
-- solve1 vs = -- maximum
--           -- . map pathLength
--    -- filter isValidPath
--    --        . map (map ((\(p,_,_) -> p) . vToK))
--    --        $ allPaths graph starts
--   traceShow (S.map projectK answer) $ calcLength answer
--   where
--     answer = maximumBy (comparing calcLength) $ concatMap search starts
--     --graphFromEdges :: Ord key => [(node, key, [key])] -> (Graph
--     --                                                     , Vertex -> (node, key, [key])
--     --                                                     , key -> Vertex)
--     calcLength = sum . S.map (foldr1 (+) . projectK)
--     projectK = (\(p,_,_) -> p) . vToK
--     nodes = (\x -> traceShow (S.size x) x)  $ S.fromList vs
--     (graph, vToK, kToV') = graphFromEdges [(v, v, neighboursOf v) | v <- vs]
--     kToV  = fromJust . kToV'
--     neighboursOf v = S.toList . S.filter (isNeighbour v) . S.delete v $ nodes
--     starts = S.toList . S.filter isStart $ nodes

--     search :: Pair Int -> [S.Set Vertex]
--     search k0 = go1 (kToV k0) (foldr1 max k0) ( S.singleton (kToV k0))
--       where
--         unseenNeighbours sn seen curr =
--           [ (nv, other)
--           | let (_,_,ns) = vToK curr
--           , n@(P x y) <- ns
--           , let nv = kToV n
--           , nv `S.notMember` seen
--           , x == sn || y == sn
--           , let other = if sn == x then y else x
--           ]

--         go1 :: Vertex -> Int -> S.Set Vertex -> [S.Set Vertex]
--         go1 curr sn seen
--           | null foo = ----traceShowId $
--             [seen]
--           | otherwise =
-- --          traceShow curr $ traceShow seen $ concat
--             concat
--           [ go1 v sn' (seen <> S.singleton v)
--           | (v, sn') <- foo-- unseenNeighbours sn seen curr
--           ]

--           where foo = unseenNeighbours sn seen curr


-- -- isValidPath :: Path (Pair Int) -> Bool
-- -- isValidPath (P 0 x : rest) = go x rest
-- --   where
-- --     go l [] = True
-- --     go l [P x y] = True
-- --     go l (P w x : P y z : rest)
-- --       | l == w && x == y = go z rest
-- --       | l == x && w == y = go z rest
-- --       | l == w && x == z = go y rest
-- --       | l == x && w == z = go y rest
-- --       | otherwise = False

main :: IO ()
main = readFile "input.txt" >>= print . (solve1 &&& solve2) . gen . readInput
