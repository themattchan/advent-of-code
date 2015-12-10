import Data.List

-- list of coordinates visited
move :: String -> [(Int,Int)]
move = foldl f [(0,0)]
  where f m@((v,h):_) '^' = (v+1,h):m
        f m@((v,h):_) 'v' = (v-1,h):m
        f m@((v,h):_) '<' = (v,h-1):m
        f m@((v,h):_) '>' = (v,h+1):m

alternate :: [a] -> ([a],[a])
alternate = foldr f ([],[])
  where f x (xs,ys) = (ys,x:xs)

uniqs :: (Eq a) => [a] -> Int
uniqs = length . nub

main :: IO ()
main = do
  is <- readFile "input.txt"
  print $ (uniqs . move) is
  let (xs,ys) = alternate is
  print $ uniqs (move xs ++ move ys)
