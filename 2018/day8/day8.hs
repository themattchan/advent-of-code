{-# LANGUAGE DeriveFunctor, DeriveFoldable,DeriveTraversable,TypeApplications #-}
import Utils
import Debug.Trace

data T a = T a [T a] deriving (Show,Functor,Foldable,Traversable)

toT = get . go
  where
    get ([t],[]) = t

    doN = foldr (>=>) pure ... replicate

    go :: [Int] -> ([T [Int]], [Int])
    go [] = ([],[])
    go (c:m:xs) =  ([T ms kids], rest2)
      where
        (kids, rest1) = doN c go xs
        (ms, rest2) = splitAt m rest1
    go x = error $ "wtf: "<> show x

t1 = sum . fold

t2 (T ms []) = sum ms
t2 (T ms kids) = foldr ((+) . get) 0 ms
  where
    lk = length kids
    get x | x > lk = 0
          | x == 0 = 0
          | otherwise = t2 (kids !! (x-1))

main = print . (t1 &&& t2) . toT . map (read @Int) . words =<< readFile "input.txt"
