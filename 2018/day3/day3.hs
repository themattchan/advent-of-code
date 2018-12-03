{-# LANGUAGE ViewPatterns #-}

import Utils
import qualified Data.RTree as RTree

mbbi :: Int -> Int -> Int -> Int -> RTree.MBB
mbbi x y x' y' = RTree.mbb (fromIntegral x) (fromIntegral y) (fromIntegral x') (fromIntegral y')

parseRT :: Parser (RTree.RTree [Int])
parseRT = do
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
  return (RTree.singleton (mbbi x y (x+w) (y+h)) [i])

main :: IO ()
main = do
  Just rt <- fmap mconcat . runParser (many parseRT) <$> readFile "input"
  print $ length [() | i <- [0..1000]
                     , j <- [0..1000]
                     , let ids = RTree.lookup (mbbi i j i j) rt
                     , length ids >= 2
                     ]
