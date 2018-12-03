-- needs package rtree
{-# LANGUAGE ViewPatterns #-}

import Utils
import qualified Data.Rtree as RTree

mbbi :: Int -> Int -> Int -> Int -> RTree.MBB
mbbi x y x' y' = RTree.mbb (fromIntegral x) (fromIntegral y) (fromIntegral x') (fromIntegral y')

parse :: Parser (RTree.RTree Int)
parse = do
  char '#'
  i <- num
  spaces
  char '@'
  spaces
  x <- num
  char ','
  y <- num
  char ':'
  spaces
  w <- num
  char 'x'
  h <- num
  spaces
  return (RTree.singleton (mbbi x y (x+w) (y+h)) i)

main :: IO ()
main = do
  Just rt <- fmap mconcat . runParser (many parse) <$> getContents
  print $ length [() | i <- [0..1000]
                     , j <- [0..1000]
                     , let ids = RTree.lookup (mbbi i j i j) rt
                     , length ids >= 2
                     ]
