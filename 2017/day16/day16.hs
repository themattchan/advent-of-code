#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

import Utils
import qualified Data.Semigroup as S

data Move = Spin Int | Exchange Int Int | Partner Char Char
  deriving Show

parseMove :: Parser [Move]
parseMove = sepBy ((pS <|> pX <|> pP) <* spaces) (char ',') where
  pS = Spin     <$> (char 's' *> num)
  pX = Exchange <$> (char 'x' *> num)   <*> (char '/' *> num)
  pP = Partner  <$> (char 'p' *> take1) <*> (char '/' *> take1)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i e = uncurry (++) . fmap ((e:) . tail) . splitAt i

solve :: [Move] -> String
solve = foldl go ['a'..'p'] where
  go st = \case
      -- list has constant length 16
      Spin i ->
        take 16 . drop (16-(i `mod` 16)) . cycle $ st
      Exchange i j ->
        replaceAt i (st!!j) . replaceAt j (st!!i) $ st
      Partner x y -> fromJust $ do
        xi <- elemIndex x st
        yi <- elemIndex y st
        return . replaceAt xi y . replaceAt yi x $ st

-- store the image of [0..n-1] under permutation
newtype Permutation = Permutation [Int]

instance S.Semigroup Permutation where
  Permutation p1 <> Permutation p2 = Permutation ((p2!!) <$> p1)

solve2 :: [Move] -> String
solve2 = unPerm . S.stimes 1000000000 . mkPerm . filter notPartner
  where
    -- label swapping is a transposition. in an even number of repetitions,
    -- this is idempotent.
    notPartner (Partner _ _) = False
    notPartner _ = True

    mkPerm = Permutation . map (subtract (ord 'a') . ord) . solve

    unPerm (Permutation out) = [chr (i + ord 'a') | i <- out]

main :: IO ()
main = do
  Just moves <- runParser parseMove <$> readFile "input.txt"
  putStrLn $ solve moves
  putStrLn $ solve2 moves
