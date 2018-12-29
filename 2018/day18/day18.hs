{-# LANGUAGE LambdaCase, BangPatterns,PatternSynonyms,FlexibleContexts,ScopedTypeVariables #-}
import qualified Data.Array as A
import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as MA
import qualified Data.Array.Base as MA
import qualified Data.Ix as Ix
import qualified Data.Map as M
import Utils

--data X = Open | Tree | Lumber deriving (Eq, Show)
pattern Open = '.'
pattern Tree = '|'
pattern Lumber = '#'

type G = A.Array (Int,Int) Char
-- indicies are (y,x)
build :: [String] -> G
build = (flip A.array <*> getBounds)
      . map assocl . foldMap sequence
      . zip [1..]                 -- y coord
      . map (zip [1..]) -- x coord
  where
    getBounds = (minimum &&& maximum) . map fst
    -- toX '.' = Open
    -- toX '|' = Tree
    -- toX '#' = Lumber
    -- toX x = error $ "wtf: " <> show x

pt :: G -> String
pt = (<> "\n") .unlines . map (foldMap (foldMap (foldMap ((:[]) . fromX)))) . groupBy ((==) `on` fst) . map assocr . A.assocs
  where
    fromX = id
  --   fromX Open = '.'
  --   fromX Tree = '|'
  --   fromX Lumber = '#'

solve :: Int -> [G] -> Int
solve n = score . (!! n)

score :: Ix.Ix ix => A.Array ix Char -> Int
score = uncurry (*) . (length . filter (==Tree) &&& length . filter (==Lumber)) . A.elems

doit :: G -> [G]
doit = iterate go
  where
    go ar = A.array bnds [ (ix, move bnds ar ix e) | (ix, e) <- A.assocs ar]
      where
        bnds = A.bounds ar

neighbours bs (y,x) = filter (Ix.inRange bs)
  [(y-1, x-1), (y-1,x), (y-1,x+1)
  ,(y, x-1),            (y,x+1)
  ,(y+1, x-1), (y+1,x), (y+1,x+1)]

move bs ar ix = let
  ns = map (ar A.!) (neighbours bs ix)
  in \case
    Open -> if length (filter (==Tree) ns) >= 3 then Tree else Open
    Tree -> if length (filter (==Lumber) ns) >= 3 then Lumber else Tree
    Lumber -> if elem Lumber ns && elem Tree ns then Lumber else Open

readMA = MA.unsafeRead
writeMA = MA.unsafeWrite
-- readMA = MA.readArray
-- writeMA = MA.writeArray

doit2 :: Int -> G -> IO G --(A.Array Int Char)
doit2 n seed = do
    let bnds@(lo,hi@(hiY, hiX)) = A.bounds seed

    let flatten (y,x) = (y-1) * hiX + (x-1)
    let expand ix = let (y,x) = ix `divMod` hiX in (y+1, x+1)
    let fbnds = (flatten lo, flatten hi)

    -- putStrLn $ "expand-flatten inverse1 : "<> show (map expand (Ix.range fbnds) == Ix.range bnds)
    -- putStrLn $ "expand-flatten inverse2 : "<> show (map flatten (Ix.range bnds) == Ix.range fbnds)
    -- putStrLn $ "expand-flatten inverse3 : "<> show (A.ixmap bnds flatten (A.ixmap fbnds expand seed) == seed)
    -- putStrLn $ "expand-flatten inverse4 : "<> show (map (neighbours bnds . expand) (Ix.range fbnds) == map (neighbours bnds) (Ix.range bnds))

    old :: MA.IOUArray Int Char <- MA.thaw $ A.ixmap fbnds expand seed -- MA.newListArray bnds as
    new :: MA.IOUArray Int Char <- MA.newArray_ fbnds

    let moveIO bs mar ix x = do
          ns <- traverse (readMA mar . flatten) (neighbours bs (expand ix))
          pure $ case x of
                   Open -> if length (filter (==Tree) ns) >= 3 then Tree else Open
                   Tree -> if length (filter (==Lumber) ns) >= 3 then Lumber else Tree
                   Lumber -> if elem Lumber ns && elem Tree ns then Lumber else Open

    let go !n old new
          | n == 0 = pure old
          | otherwise = do
              for_ (Ix.range fbnds) $ \ix ->
                readMA old ix
                  >>= moveIO bnds old ix
                  >>= writeMA new ix
              go (n-1) new old

    fz <- go n old new >>= MA.freeze
    pure $ A.ixmap bnds flatten fz

-- x, y, z .... [ LOOP ] [ LOOP ] ..... [LOOP, ..., 1000000000.
findLoop :: [G] -> (Int, Int, G)
findLoop = go mempty 0
  where
    go ss !n (g:gs)
      | Just k <- M.lookup g ss = (n-k, n, g)
      | otherwise = go (M.insert g n ss) (n+1) gs

main = do
  i <- build . lines <$> readFile "input"
--  traverse_ (putStrLn . pt) (take 11 (doit i))
  print (solve 10 (doit i))
  let (period, endOfLoop, g) = findLoop (doit i)

--  print lp
  let n =  (1000000000 - endOfLoop) `mod` period

  -- let scores = map score (doit i)
  -- print $ take 1000 scores
  print $ solve n (doit g)
--  print n


  -- first10 <- traverse (\n -> doit2 n i) [0..1]
  -- putStrLn "================================================================================"
  -- putStrLn "GOOD: "
  -- traverse_ (putStrLn . pt) (take 2 (doit i))
  -- putStrLn "================================================================================"
  -- putStrLn "BAD: "
  -- traverse_ (putStrLn. pt) first10
  -- print $ "doit == doit2: " <> show (map pt (take 2 (doit i)) == map pt first10)

--  doit2 10 i >>= print . score

--  doit2 1000000000 i >>= print . score
