#! /usr/bin/env runhaskell -i../
import Utils

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
