module Zipper where

newtype Zipper a = Zipper ([a], a, [a]) deriving Show

zipLeft :: Int -> Zipper a -> Maybe (Zipper a)
zipLeft 0 z = Just z
zipLeft n (Zipper ([],_,_)) = Nothing
zipLeft n (Zipper (l:ls,e,rs)) = zipLeft (n-1) (Zipper (ls, l, e:rs))

zipRight :: Int -> Zipper a -> Maybe (Zipper a)
zipRight 0 z = Just z
zipRight n (Zipper (_,_,[])) = Nothing
zipRight n (Zipper (ls,e,r:rs)) = zipRight (n-1) (Zipper (e:ls, r, rs))

move :: Int -> Zipper a -> Maybe (Zipper a)
move n | n > 0     = zipRight n
       | otherwise = zipLeft (abs n)

viewFoci :: Zipper a -> a
viewFoci (Zipper (_,e,_)) = e

modifyFoci :: (a -> a) -> Zipper a -> Zipper a
modifyFoci f (Zipper (ls, e, rs)) = Zipper (ls, f e, rs)

zipperFromList :: [a] -> Zipper a
zipperFromList []     = undefined
zipperFromList (x:xs) = Zipper ([], x, xs)
