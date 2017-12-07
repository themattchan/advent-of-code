#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE DeriveFunctor, PatternGuards #-}
import Utils
import Debug.Trace
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

annot :: Monoid m => (a -> m) -> Rose a -> Rose (m, a)
annot f (Rose a as) = Rose (m, a) as'
  where
    m   = f a <> foldMap (fst . roseRoot) as'
    as' = map (annot f) as

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
