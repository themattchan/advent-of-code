module Utils
  ( module Utils
  , module Control.Applicative
  , module Control.Monad
  , module Data.Bifunctor
  , module Data.Char
  , module Data.Maybe
  , module Data.Monoid
  , module Data.List
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.List

(...) = (.).(.)

--------------------------------------------------------------------------------

-- "A parser for things is a function from strings to a list of pairs of things and strings"
newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser = (fmap fst . mfilter (null.snd) . listToMaybe) ... parse

instance Functor Parser where
  fmap f p = Parser $ map (first f) . parse p

instance Applicative Parser where
  pure x = Parser $ \s -> [(x,s)]
  pf <*> pa = Parser $ \s -> [ (f a, s'') | (f,s') <- parse pf s, (a,s'') <- parse pa s']

instance Monad Parser where
  p >>= f = Parser $ \s -> concat [ parse (f a) s' | (a,s') <- parse p s ]

instance MonadPlus Parser where
  mzero = Parser $ const []
  p `mplus` q = Parser $ \s -> parse p s <> parse q s

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ \s ->
    let f = parse p s in
      if null f then parse q s else f

take1 = Parser $ maybeToList . uncons

satisfy = flip mfilter take1

oneof = satisfy . flip elem

char = satisfy . (==)

sepBy p s = many ((p <* s) <|> p)

num :: Parser Int
num = read <$> some (satisfy isDigit)

spaces = many (oneof "\t\n ")

string :: String -> Parser String
string = mapM char

literal s x = string s >> pure x

eat = flip replicateM_ take1
