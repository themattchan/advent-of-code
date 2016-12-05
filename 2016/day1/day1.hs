import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.List

--------------------------------------------------------------------------------

-- A parser for things is a function from strings to a list of things and strings
newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser p s = case parse p s of
  (r, ""):_ -> Just r
  _         -> Nothing

instance Functor Parser where
  fmap f p = Parser $ map (first f) . parse p

instance Applicative Parser where
  pure x = Parser $ \s -> [(x,s)]
  pf <*> pa = Parser $ \s -> [ (f a, s'') | (f,s') <- parse pf s, (a,s'') <- parse pa s']

instance Monad Parser where
  p >>= f = Parser $ \s -> concat [ parse (f a) s' | (a,s') <- parse p s]

instance MonadPlus Parser where
  mzero = Parser $ const []
  p `mplus` q = Parser $ \s -> parse p s <> parse q s

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ \s ->
    let  f = parse p s in
      if null f then parse q s else f

take1 = Parser $ maybeToList . uncons

satisfy = flip mfilter take1

oneof ts = satisfy (`elem` ts)

char = satisfy . (==)

sepBy p s = many ((p <* s) <|> p)

num = read <$> some (satisfy isDigit)

spaces = many (oneof "\t\n ")

--------------------------------------------------------------------------------


data Turn = L | R deriving Show
data Mov = Mov Turn Int deriving Show

parseInput :: Parser [Mov]
parseInput = pmov `sepBy` (char ',' >> spaces)
  where pmov = Mov
            <$> ((char 'L' *> pure L) <|> (char 'R' *> pure R))
            <*> num

--------------------------------------------------------------------------------

data Dir = N | E | S | W
  deriving (Eq, Show)

-- No. of blocks: N E S W
data State = State Dir Int Int Int Int
  deriving Show

initialState = State N 0 0 0 0

move1 :: State -> Mov -> State
move1 (State d n e s w) (Mov turn  step) = undefined

main :: IO ()
main = do
  x <- runParser parseInput <$> readFile "input.txt"
  case x of
    Nothing -> error "parse failure"
    Just movs -> print movs
      -- let finState = foldl' move1 initialState movs
      -- in print finState
