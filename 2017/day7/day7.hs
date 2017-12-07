#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE DeriveFunctor #-}
import Utils
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data TowerId = Id { idName :: String, idNum :: Int }
  deriving (Show, Eq, Ord)

data Tower a = Tower { towerId :: TowerId, towerKids :: [a] }
  deriving (Functor, Show)

parseInput :: Parser (Tower (M.Map String TowerId -> TowerId))
parseInput = line where
  line = do
    name <- alpha
    char ' '
    id <- parens num
    ks <- kids <|> pure []
    return $ Tower (Id name id) ks

  kids = string " -> " *> takeAll >>= pure . map (fromJust ... M.lookup) . splitOn ", "

resolve :: [Tower (M.Map String TowerId -> TowerId)] -> [Tower TowerId]
resolve ts = map (fmap ($ towerMap)) ts
  where
    towerMap = M.fromList . map ((idName &&& id) . towerId) $ ts

findRootName :: [Tower TowerId] ->
findRootName = S.findMin . uncurry S.difference . (allIds &&& allChildren)
  where
    allIds = S.fromList . map towerId
    allChildren = S.fromList . concatMap towerKids

solve1 = fmap (findRoot . resolve) . traverse (runParser parseInput)

main :: IO ()
main = do
  i <- lines <$> readFile "input.txt"
  print $ (fmap length . parseInputs) i == Just 1454
  print $ solve i
