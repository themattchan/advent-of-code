#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE DeriveFunctor, PatternGuards #-}
import Utils
import Debug.Trace
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Semigroup hiding ((<>))

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

annot :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
annot f (Rose a as) = Rose (m, a) as'
  where
    m   = f a <> foldMap (fst . roseRoot) as'
    as' = map (annot f) as

-- diff ::  Monoid m => (a -> m) -> Rose a -> Rose ((m, m), a)
-- diff f (Rose a as) = Rose (m', a) as'
--   where
--     m'  = (m <> f a, m)
--     m   = foldMap (fst . fst . roseRoot) as'
--     as' = map (diff f) as

-- annot :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
-- annot f = fmap (first fst) . diff f

propagate :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
propagate f (Rose a as) = Rose (m, a) as'
  where
    m   = foldMap (f . roseRoot) as <> foldMap (fst . roseRoot) as'
    as' = map (propagate f) as

-- Summarise info about a node's children to a node
levelUp :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
levelUp f (Rose a as) = Rose (m, a) as'
 where
   m   = foldMap (f . roseRoot) as
   as' = map (levelUp f) as

paths :: Rose a -> [[a]]
paths (Rose a []) = [[a]]
paths (Rose a as) = [ a : ps | ps <- foldMap paths as ]

testRose :: Rose Int
testRose = Rose 1 [ Rose 2 [Rose 3 []]
                  , Rose 4 []
                  , Rose 5 [Rose 6 [Rose 7 []]]]

-- Monoid to track differences

newtype Diff a = Diff (M.Map a (Sum Int))
  deriving (Ord, Eq, Bounded)

instance (Bounded a, Ord a, Eq a) => Show (Diff a) where
  show = show . isDiff

-- propagate (mkDiff . fst) . annot (Sum . idWeight) . structure $ test

mkDiff :: (Bounded a, Ord a, Eq a) => a -> Diff a
mkDiff x = Diff (Min x, Max x, M.singleton x (Sum 1))

isDiff :: (Bounded a, Ord a, Eq a) => Diff a -> Bool
isDiff d@(Diff (Min x, Max y))
  | d == mempty = False
  | x == y      = False
  | otherwise   = True

getDiff :: (Num a, Bounded a, Ord a, Eq a) => Diff a -> a
getDiff (Diff (Min x, Max y)) = y - x

instance (Eq a, Ord a, Bounded a) => Monoid (Diff a) where
  mempty = Diff (mempty, mempty, mempty)
  Diff (min1, max1, counts1) `mappend` Diff (min2, max2, counts2)
    = Diff (min1 <> min2, max1 <> max2)

-- turn (Sum Int, Id) into a diffable thing

-- * Part 2

structure :: [Tower Id] -> Rose Id
structure = go . (findRoot &&& M.fromList . map (towerId &&& towerKids))
  where
    go (root, nodes) = Rose root [ go (kid, nodes)
                                 | Just kids <- [M.lookup root nodes]
                                 , kid       <- kids
                                 ]

--foo = propagate . annot (Sum . idWeight)

findDiscrepancy :: Rose Id -> Maybe Int
findDiscrepancy = go . roseLevel . annot (Sum . idWeight)
  where
    subtreeWeight = fst . roseRoot
    nodeWeight = idWeight . snd . roseRoot

    findBad = classify . f
      where
        cmp = fst . roseRoot
        f = map head . sortOn length . groupBy ((==) `on` cmp) . sortOn cmp
        classify xs
          | [bad, good] <- xs = Just (bad, good)
          | [_good]     <- xs = Nothing
          | otherwise         = error "BOOM"

    go level =
      -- First, find the bad one in this list of nodes
      case findBad level of
        Just (bad, good) ->
          -- Next, check if the bad one is in this node or its children
          case findBad (roseLevel bad) of
            Just (bad1, good1) -> go (roseLevel bad1)
            Nothing -> let d = subtreeWeight bad - subtreeWeight good
                       in Just (nodeWeight bad - getSum d)

        -- This list is clean
        Nothing -> Nothing

--- TODO should have done this with another monoid... sigh i shouldn't code at 3am
-- annot, then PROPAGATE annots with a "diff" monoid

test = fromJust . towers $ unlines
  [ "pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj, cntj, xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga, havc, qoyq"
  , "tknk (41) -> ugml, padx, fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo, ebii, jptl"
  , "gyxo (61)"
  , "cntj (57)"
  ]



main :: IO ()
main = do
  Just i <- towers <$> readFile "input.txt"
  print $ findRoot i
  print $ (findDiscrepancy . structure) i
