{-# LANGUAGE TupleSections #-}
module Utils
  ( module Utils
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Char
  , module Data.Either
  , module Data.Function
  , module Data.Foldable
--  , module Data.Semigroup
  , module Data.Tuple
  , module Data.Maybe
  , module Data.Monoid
  , module Data.List
  , module Data.Ord
  ) where

import Control.Applicative
import Control.Arrow ((***), (&&&), (>>>), (<<<))
import Control.Monad
import Data.Bits
import Data.Bifunctor
import Data.Bool (bool)
import Data.Char
import Data.Either
import Data.Function (on, (&))
import Data.Foldable
--import Data.Semigroup
import Data.Tuple (swap)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.List
import Data.Ord (comparing)
import Text.Printf
import System.CPUTime
import Numeric (showHex, showIntAtBase)

--------------------------------------------------------------------------------
-- * Algebra

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.).(.)
{-# INLINE (...) #-}
infixl 8 ...

dup :: a -> (a, a)
dup = id &&& id

assocl :: (a, (b, c)) -> ((a, b), c)
assocl ~(a, (b, c)) = ((a, b), c)

assocr :: ((a, b), c) -> (a, (b, c))
assocr ~((a, b), c) = (a, (b, c))

square :: (a -> b) -> (a, a) -> (b, b)
square f (x, y) = (f x, f y)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

window :: Int -> [a] -> [[a]]
window n xs = take (length xs - n + 1)
            $ unfoldr (Just . (take n &&& tail)) (cycle xs)

tally :: (Foldable t, Ord a) => t a -> M.Map a Int
tally = foldr (\x m -> M.insertWith (+) x 1 m) mempty
{-# INLINEABLE tally #-}
{-# SPECIALISE tally :: [Int] -> M.Map Int Int #-}
{-# SPECIALISE tally :: [String] -> M.Map String Int #-}

--------------------------------------------------------------------------------
-- * Parser combinators

-- | "A parser for things is a function from strings to a list of pairs of things and strings"
newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> Maybe a
runParser = fmap fst . mfilter (null.snd) . listToMaybe ... parse

runParserPartial :: Parser a -> String -> Maybe a
runParserPartial = fmap fst . listToMaybe ... parse

instance Functor Parser where
  fmap f p = Parser $ map (first f) . parse p

instance Applicative Parser where
  pure = Parser . (pure ... (,))
  pf <*> pa = Parser $ concatMap (uncurry (fmap.first))
                     . map (id *** parse pa) . parse pf

joinParser :: Parser (Parser a) -> Parser a
joinParser = Parser . (concatMap (uncurry parse) ... parse)

instance Monad Parser where
  (>>=) = joinParser ... flip fmap

instance MonadPlus Parser where
  mzero = Parser $ const []
  p `mplus` q = Parser $ uncurry mappend . (parse p &&& parse q)

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ uncurry (<|>) . (parse p &&& parse q)

take1 :: Parser Char
take1 = Parser $ maybeToList . uncons

takeN :: Int -> Parser String
takeN = flip replicateM take1

satisfy :: (Char -> Bool) -> Parser Char
satisfy = flip mfilter take1

oneof :: String -> Parser Char
oneof = satisfy . flip elem

char :: Char -> Parser Char
char = satisfy . (==)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = many ((p <* s) <|> p)

num :: Parser Int
num = read <$> ((++) <$> (string "-" <|> pure "") <*> some (satisfy isDigit))

spaces :: Parser String
spaces = many (oneof "\t\n ")

alpha :: Parser String
alpha = many (oneof ['a'..'z'])

string :: String -> Parser String
string = mapM char

literal :: String -> a -> Parser a
literal = curry (uncurry (*>) . (string *** pure))

eat :: Int -> Parser ()
eat = flip replicateM_ take1

between :: Parser a -> Parser b -> Parser c -> Parser c
between p q = (p *>) . (<* q)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

takeAll :: Parser String
takeAll = Parser $ pure . (, "")

--------------------------------------------------------------------------------
-- * Misc

myShowBin, myShowHex :: (Integral a, PrintfArg a, FiniteBits a) => a -> String
myShowHex x = printf "%0*Lx" (finiteBitSize x) x
myShowBin x = printf "%0*Lb" (finiteBitSize x) x

showHex' :: (Integral a, Show a) => a -> String
showHex' n = padZero 2 $ showHex n ""

showIntAtBase' :: (Integral a, Show a) => Int -> a -> a -> String
showIntAtBase' w b n = padZero w $ showIntAtBase b intToDigit n ""

padZero :: Int -> String -> String
padZero w s = replicate (w - length s) '0' ++ s

numbers :: String -> [[Int]]
numbers = map (map read . words) . lines

-- from Haskell wiki
timed :: IO a -> IO a
timed action = do
  start <- getCPUTime
  v <- action
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v
