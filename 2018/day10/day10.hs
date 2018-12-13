{-# LANGUAGE BangPatterns #-}
import Utils
import qualified Data.IntMap as M

data P = P {x,y,dx,dy:: !Int} deriving (Show, Eq)

instance Ord P where
  (P x1 y1 _ _ ) `compare` (P x2 y2 _ _ ) =
    case compare y1 y2 of
      EQ -> compare x1 x2
      x -> x

boundingBox :: [P] -> (Int,Int)
boundingBox ps = (maxx - minx, maxy-miny)
  where
    minx = minimum $ map x ps
    maxx = maximum $ map x ps
    miny = minimum $ map y ps
    maxy = maximum $ map y ps

normalise :: [P] -> [P]
normalise ps = map go ps
  where
    go (P x y dx dy) = P (x-minx) (y-miny) dx dy
    minx = minimum $ map x ps
    miny = minimum $ map y ps

shrink :: Int -> [P] -> (Int, [P])
shrink !i ps
--  | bps <= (50,50), boundingBox ps' >= bps = ps
  | snd (boundingBox ps) <= 10 = (i, ps)
  | otherwise =  shrink (i+1) (map step ps)
  where
--    bps = boundingBox ps
    step (P x y dx dy) = P (x+dx) (y+dy) dx dy

doit = unlines . uncurry (:) . bimap show (pp .  normalise) . shrink 0
  where
    pp ps = [ [ if isJust (M.lookup y pointsMap >>= find (==x)) then '#' else '.'
              | x <- [minx..maxx]
              ]
            | y <- [miny..maxy]
            ]
      where
        pointsMap = M.fromList [ (y (head g), map x g) | g <- groupBy ((==) `on` y) (sort ps)]

        minx = minimum $ map x ps
        maxx = maximum $ map x ps
        miny = minimum $ map y ps
        maxy = maximum $ map y ps

parse1 :: String -> P
parse1 s = P (f 1) (f 2) (f 3) (f 4)
  where
    rgx = makeRegex "^position=< *(-?[0-9]+), *(-?[0-9]+)> *velocity=< *(-?[0-9]+), *(-?[0-9]+)>$"
    f = read . match' rgx s

main = putStrLn . doit . map parse1 . lines =<< readFile "input"
