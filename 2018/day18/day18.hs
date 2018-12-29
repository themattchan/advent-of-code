{-# LANGUAGE LambdaCase, BangPatterns,PatternSynonyms,FlexibleContexts,ScopedTypeVariables #-}
import qualified Data.Array as A
import qualified Data.Ix as Ix
import qualified Data.Map as M
import Utils

pattern Open = '.'
pattern Tree = '|'
pattern Lumber = '#'

type G = A.Array (Int,Int) Char

-- indicies are (y,x)
build :: [String] -> G
build = (flip A.array <*> getBounds)
      . map assocl . foldMap sequence
      . zip [1..]                 -- y coord
      . map (zip [1..])           -- x coord
  where
    getBounds = (minimum &&& maximum) . map fst

pt :: G -> String
pt = (<> "\n") . unlines . map (map (snd . snd)) . groupBy ((==) `on` fst) . map assocr . A.assocs


solve :: Int -> [G] -> Int
solve n = score . (!! n)

score :: Ix.Ix ix => A.Array ix Char -> Int
score = uncurry (*) . (length . filter (==Tree) &&& length . filter (==Lumber)) . A.elems

doit :: G -> [G]
doit = iterate go
  where
    go ar = A.array bnds [ (ix, move ar ix e) | (ix, e) <- A.assocs ar]
      where
        bnds = A.bounds ar

        neighbours (y,x) = filter (Ix.inRange bnds)
          [(y-1, x-1), (y-1,x), (y-1,x+1)
          ,(y, x-1),            (y,x+1)
          ,(y+1, x-1), (y+1,x), (y+1,x+1)]

        move ar ix = let
          ns = map (ar A.!) (neighbours ix)
          in \case
            Open -> if length (filter (==Tree) ns) >= 3 then Tree else Open
            Tree -> if length (filter (==Lumber) ns) >= 3 then Lumber else Tree
            Lumber -> if elem Lumber ns && elem Tree ns then Lumber else Open

-- x, y, z .... [ LOOP ] [ LOOP ] ..... [LOOP, ..., 1000000000.
findLoop :: [G] -> (Int, Int, G)
findLoop = go mempty 0
  where
    go ss !n (g:gs)
      | Just k <- M.lookup g ss = (n-k, n, g)
      | otherwise = go (M.insert g n ss) (n+1) gs

main = do
  i <- build . lines <$> readFile "input"
  print (solve 10 (doit i))
  let (period, end, g) = findLoop (doit i)
  let n =  (1000000000 - end) `mod` period
  print $ solve n (doit g)
