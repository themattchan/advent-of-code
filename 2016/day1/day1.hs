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
  deriving (Show, Eq, Ord)

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

type XY = (Int,Int)

toXY (State _ n e s w) = (e-w, n-s)

type Seg = (XY,XY)

-- http://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/
-- def ccw(A,B,C):
--     return (C.y-A.y)*(B.x-A.x) > (B.y-A.y)*(C.x-A.x)
-- def intersect(A,B,C,D):
--     return ccw(A,C,D) != ccw(B,C,D) and ccw(A,B,C) != ccw(A,B,D)

ccw :: XY -> XY -> XY -> Bool
ccw (ax,ay) (bx,by) (cx,cy) = (cy-ay) * (bx-ax) > (by-ay) * (cx-ax)

intersect :: Seg -> Seg -> Bool
intersect (a,b) (c,d) = (ccw a c d /= ccw b c d) && (ccw a b c /= ccw a b d)

intersects :: Seg -> [Seg] -> Maybe Seg
intersects s = foldr go Nothing
  where
    go Nothing ss | intersects s ss = Just ss
                  | otherwise = Nothing
    go j _ = j

part2 (m:movs) = foldl' go ini movs
  where
    ini = Left (initialState, toXY initialState, [])
    go (Left (st, lastSt, segs)) x
      | Just ss <- intersects newSeg segs
      = Right
      | otherwise = Left (st', toXY st, newSeg:segs)
      where
        st' = move st x
        newSeg = (toXY st,lastSt)

    go r _ = r

main :: IO ()
main = do
  x <- runParser parseInput <$> readFile "input.txt"
  case x of
    Nothing -> error "parse failure"
    Just movs -> do
      print $ distanceMoved (foldl' move initialState movs)
      print $ distanceMoved (part2 movs)
