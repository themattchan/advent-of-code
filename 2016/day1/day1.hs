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

num = read <$> some (satisfy isDigit)

spaces = many (oneof "\t\n ")

--------------------------------------------------------------------------------

data Turn = L | R deriving Show
data Mov = Mov Turn Int deriving Show

parseInput :: Parser [Mov]
parseInput = (pmov `sepBy` (char ',' >> spaces)) <* spaces
  where pmov = Mov
            <$> ((char 'L' *> pure L) <|> (char 'R' *> pure R))
            <*> num

--------------------------------------------------------------------------------

data Dir = N | E | S | W
  deriving (Eq, Show, Enum, Bounded)

next :: Dir -> Dir
next e | e == maxBound = minBound
       | otherwise = succ e

prev :: Dir -> Dir
prev e | e == minBound = maxBound
       | otherwise = pred e

data State = State Dir Int Int Int Int
  deriving Show

initialState = State N 0 0 0 0

step (State N n e s w) i = (State N (n+i) e s w)
step (State E n e s w) i = (State E n (e+i) s w)
step (State S n e s w) i = (State S n e (s+i) w)
step (State W n e s w) i = (State W n e s (w+i))

turn (State dir n e s w) L = (State (prev dir) n e s w)
turn (State dir n e s w) R = (State (next dir) n e s w)

distanceMoved (State _ n e s w) = abs (n + e - s - w)

move :: State -> Mov -> State
move st (Mov tu n) = step (turn st tu) n

main :: IO ()
main = do
  x <- runParser parseInput <$> readFile "input.txt"
  case x of
    Nothing -> error "parse failure"
    Just movs -> print $ distanceMoved (foldl' move initialState movs)
