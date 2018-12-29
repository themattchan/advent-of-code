{-# LANGUAGE LambdaCase #-}
import qualified Data.Array as A
import qualified Data.Ix as Ix
import Utils

data X = Open | Tree | Lumber deriving (Eq, Show)

type G = A.Array (Int,Int) X
-- indicies are (y,x)
build :: [String] -> G
build = (flip A.array <*> getBounds)
      . map assocl . foldMap sequence
      . zip [1..]                 -- y coord
      . map (zip [1..] . map toX) -- x coord
  where
    getBounds = (minimum &&& maximum) . map fst
    toX '.' = Open
    toX '|' = Tree
    toX '#' = Lumber
    toX x = error $ "wtf: " <> show x

pt :: G -> String
pt = (<> "\n") .unlines . map (foldMap (foldMap (foldMap ((:[]) . fromX)))) . groupBy ((==) `on` fst) . map assocr . A.assocs
  where
    fromX Open = '.'
    fromX Tree = '|'
    fromX Lumber = '#'

solve :: Int -> [G] -> Int
solve n = score . (!! n)
  where
    score = uncurry (*) . (length . filter (==Tree) &&& length . filter (==Lumber)) . A.elems

-- accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
doit :: G -> [G]
doit = iterate go
  where
    go ar = A.array bnds [ (ix, move ix e) | (ix, e) <- A.assocs ar]
      where
        bnds = A.bounds ar
        neighbours (y,x) = filter (Ix.inRange bnds)
          [(y-1, x-1), (y-1,x), (y-1,x+1)
          ,(y, x-1), (y,x+1)
          ,(y+1, x-1), (y+1,x), (y+1,x+1)]

        move ix = let
          ns = map (ar A.!) (neighbours ix)
          in \case
            Open -> if length (filter (==Tree) ns) >= 3 then Tree else Open
            Tree -> if length (filter (==Lumber) ns) >= 3 then Lumber else Tree
            Lumber -> if elem Lumber ns && elem Tree ns then Lumber else Open

main = do
  i <- doit . build . lines <$> readFile "input"
--  traverse_ (putStrLn . pt) (take 11 (doit i))
  print (solve 10 i)
  print (solve 1000000000 i)
