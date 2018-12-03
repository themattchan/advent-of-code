{-# LANGUAGE TypeApplications, ViewPatterns #-}

import Utils
import qualified Data.RTree as R
import qualified Data.RTree.Base as R
import qualified Data.RTree.MBB as R
import qualified Data.IntSet as S
import Debug.Trace

mbbi :: Int -> Int -> Int -> Int -> R.MBB
mbbi x y x' y' = R.mbb (fromIntegral x) (fromIntegral y) (fromIntegral x') (fromIntegral y')

parseIt :: String -> R.RTree [Int]
parseIt = foldMap go . lines
  where
    go = toRT . map (read @Int) . words . map clean
    clean x | x `elem` "#@,:x" = ' '
            | otherwise = x
    toRT [i,x,y,w,h] = R.singleton (mbbi x y (x+w) (y+h)) [i]
    toRT x = error $ "toRT: "<>show x

--
-- MODIFIED FROM LIBRARY: lookupRangeWithKey, lookupRange
--

-- | returns all keys and values, which contain the given point
lookupPointWithKey :: R.MBB -> R.RTree a -> [(R.MBB, a)]
lookupPointWithKey _ R.Empty = []
lookupPointWithKey pt t@R.Leaf{}
    | (R.getMBB t) `R.containsMBB` pt = [(R.getMBB t, R.getElem t)]
    | otherwise = []
lookupPointWithKey pt t = founds
  where
    matches = filter intersectRTree $ R.getChildren t
    founds = concatMap (lookupPointWithKey pt) matches
    intersectRTree x = isJust $ (R.getMBB x) `R.intersectMBB` pt

-- | returns all values, which are located in the given bounding box.
lookupPoint :: R.MBB -> R.RTree a -> [a]
lookupPoint pt t = snd <$> (lookupPointWithKey pt t)

--
-- END MODFIED FROM LIBRARY
--


main :: IO ()
main = do
  rt <- parseIt <$> readFile "input"
--  print rt
--  print $ R.keys rt
--  print $ R.values rt
  let overlaps = [ids
                 | i <- [0..1000]
                 , j <- [0..1000]
                 , let ids = lookupPoint (mbbi i j (i+1) (j+1)) rt
                 , length ids >= 2
                 ]
  print (length overlaps)
  print (foldMap S.fromList (R.values rt) `S.difference` foldMap (foldMap S.fromList) overlaps)
