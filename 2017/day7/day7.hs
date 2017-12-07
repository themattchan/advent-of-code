#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE DeriveFunctor #-}
import Utils
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- * Data types

data Tower a = Tower { towerId :: Id, towerKids :: [a] }
  deriving (Show, Functor)

data Id = Id { idName :: String, idWeight :: Int }
  deriving (Show, Eq, Ord)

-- * Parsing

towers :: String -> Maybe [Tower Id]
towers = fmap resolve . traverse (runParser parseInput) . lines

parseInput :: Parser (Tower (M.Map String Id -> Id))
parseInput = line where
  line = do
    name <- alpha
    char ' '
    weight <- parens num
    ks <- kids <|> pure []
    return $ Tower (Id name weight) ks

  kids = string " -> " *> takeAll >>= pure . map (fromJust ... M.lookup) . splitOn ", "

resolve :: [Tower (M.Map String Id -> Id)] -> [Tower Id]
resolve = uncurry map . ( (fmap . flip ($) . towerMap) &&& id )
  where
    towerMap = M.fromList . map ((idName &&& id) . towerId)

-- * Part 1

findRoot :: [Tower Id] -> Id
findRoot = S.findMin . uncurry S.difference . (allIds &&& allKids)
  where
    allIds  = S.fromList . map towerId
    allKids = S.fromList . foldMap towerKids

-- * Rose trees

data Rose a = Rose a [Rose a]
  deriving (Show, Functor)

roseRoot :: Rose a -> a
roseRoot (Rose e _) = e

roseLevel :: Rose a -> [Rose a]
roseLevel (Rose e ls) = ls

-- annot :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
-- annot f (Rose a as) = Rose (m, a) as'
--   where
--     m   = f a <> foldMap (fst . roseRoot) as'
--     as' = map (annot f) as

propagate :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
propagate f (Rose a as) = Rose (m, a) as'
  where
    m   = foldMap (f . roseRoot) as <> foldMap (fst . roseRoot) as'
    as' = map (propagate f) as

diff ::  Monoid m => (a -> m) -> Rose a -> Rose ((m, m), a)
diff f = fmap add . propagate f
  where
    add (m, a) = ((m <> f a, m), a)

annot :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
annot f = fmap (first fst) . diff f

paths :: Rose a -> [[a]]
paths (Rose a []) = [[a]]
paths (Rose a as) = [ a : ps | ps <- foldMap paths as ]

testRose = Rose 1 [ Rose 2 [Rose 3 []]
                  , Rose 4 []
                  , Rose 5 [Rose 6 [Rose 7 []]]]

-- * Part 2

structure :: [Tower Id] -> Rose Id
structure = go . (findRoot &&& M.fromList . map (towerId &&& towerKids))
  where
    go (root, nodes) = Rose root [ go (kid, nodes)
                                 | Just kids <- [M.lookup root nodes]
                                 , kid       <- kids
                                 ]

findDiscrepancy :: Rose Id -> Int
findDiscrepancy = diff (Sum . idWeight)


main :: IO ()
main = do
  Just i <- towers <$> readFile "input.txt"
  print $ weighted $ structure i
  print $ findRoot i
