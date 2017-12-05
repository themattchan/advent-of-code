#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections #-}

import Utils

newtype Zipper a = Zipper ([a], a, [a])

instance Functor Zipper where
  fmap f (Zipper (ls, e, rs)) = Zipper (fmap f ls, f e, fmap f rs)

zipLeft :: Int -> Zipper a -> Maybe (Zipper a)
zipLeft 0 z = Just z
zipLeft n z@(Zipper ([],_,_)) = Nothing --Just z
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

zipperFromList (x:xs) = Zipper ([], x, xs)

-- this is a bit slow
solve1 :: Zipper Int -> Int
solve1 = (+1) . length . unfoldr go where
  go z = let n = viewFoci z
             z' = modifyFoci (+1) z
         in ((), ) <$> move n z'

main :: IO ()
main = do
  zipper <- zipperFromList . map read . lines <$> getContents
  print $ solve1 zipper
