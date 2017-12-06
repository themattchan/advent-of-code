module Utils
  ( module Utils
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Char
  , module Data.Function
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
import Data.Function (on)
import Data.Tuple (swap)
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Ord (comparing)
import Text.Printf
import System.CPUTime
import qualified Numeric

infixl 8 ...
(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.).(.)

showBin, showHex :: (Integral a, PrintfArg a, FiniteBits a) => a -> String
showHex x = printf "%0*Lx" (finiteBitSize x) x
showBin x = printf "%0*Lb" (finiteBitSize x) x

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

dup :: a -> (a, a)
dup = id &&& id

--------------------------------------------------------------------------------

-- "A parser for things is a function from strings to a list of pairs of things and strings"
newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser = fmap fst . mfilter (null.snd) . listToMaybe ... parse

runParserPartial = fmap fst . listToMaybe ... parse

instance Functor Parser where
  fmap f p = Parser $ map (first f) . parse p

instance Applicative Parser where
  pure = Parser . (pure ... (,))
  pf <*> pa = Parser $ concatMap (uncurry (fmap.first))
                     . map (id *** parse pa) . parse pf

joinParser = Parser . (concatMap (uncurry parse) ... parse)
instance Monad Parser where
  (>>=) = joinParser ... flip fmap

instance MonadPlus Parser where
  mzero = Parser $ const []
  p `mplus` q = Parser $ uncurry mappend . (parse p &&& parse q)

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ uncurry (<|>) . (parse p &&& parse q)

take1 = Parser $ maybeToList . uncons

satisfy = flip mfilter take1

oneof = satisfy . flip elem

char = satisfy . (==)

sepBy p s = many ((p <* s) <|> p)

num :: Parser Int
num = read <$> some (satisfy isDigit)

spaces = many (oneof "\t\n ")

alpha :: Parser String
alpha = many (oneof ['a'..'z'])

string :: String -> Parser String
string = mapM char

literal s x = string s *> pure x

eat = flip replicateM_ take1

between p q = (p *>) . (<* q)
