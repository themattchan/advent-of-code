#! /usr/bin/env runhaskell -i../../
import Utils
import qualified Data.Set as S

--------------------------------------------------------------------------------

data Turn = L | R deriving Show
data Mov = Mov Turn Int deriving Show

parseInput :: Parser [Mov]
parseInput = (pmov `sepBy` (char ',' >> spaces)) <* spaces
  where pmov = Mov
            <$> (literal "L" L <|> literal "R" R)
            <*> num

--------------------------------------------------------------------------------

data Dir = N | E | S | W
  deriving (Eq, Ord, Show, Enum, Bounded)

next :: Dir -> Dir
next e | e == maxBound = minBound
       | otherwise = succ e

prev :: Dir -> Dir
prev e | e == minBound = maxBound
       | otherwise = pred e

data State = State Dir Int Int Int Int
  deriving (Show, Ord)

instance Eq State where
  State _ n1 e1 s1 w1 == State _ n2 e2 s2 w2
   = and $ zipWith (==) [n1,e1,s1,w1] [n2,e2,s2,w2]

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

part2 =  map distanceMoved . scanl move initialState
  where go (x : xs) = if x `elem` xs then x else go xs
        go  _ = undefined


main :: IO ()
main = do
  x <- runParser parseInput <$> readFile "input.txt"
  case x of
    Nothing -> error "parse failure"
    Just movs -> do
      print $ distanceMoved (foldl' move initialState movs)
      print $ (part2 movs)
