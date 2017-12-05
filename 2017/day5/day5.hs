#! /usr/bin/env runhaskell -i../../

{-# LANGUAGE
    TupleSections
  , BangPatterns
  , MagicHash
  , UnboxedTuples
  , ScopedTypeVariables
#-}

-- Compile with:
--    stack ghc -- day5.hs -i ../../Utils.hs

import Utils
import qualified Data.Vector.Mutable as V
import Data.IORef
import Data.Int
import GHC.IO(IO(IO))

newtype Zipper a = Zipper ([a], a, [a])

instance Functor Zipper where
  fmap f (Zipper (ls, e, rs)) = Zipper (fmap f ls, f e, fmap f rs)

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

solve :: (Int -> Int) -> Zipper Int -> Int
solve f = foldl' (+) 1 . unfoldr go
  where
    go = fmap (1, ) . uncurry move . (viewFoci &&& modifyFoci f)

--func :: Int -> Int -> Int
--func = uncurry (bool (+ 1) (- 1) . (>= 3)) . fmap (flip ($)) .  dup
-- (uncurry (bool (+ 1) (- 1) . (>= 3)) . fmap (flip ($)) .  dup)

part2Func :: Integral a => a -> a
part2Func x | x >= 3    = x-1
            | otherwise = x+1

main1 :: IO ()
main1 = do
  zipper <- zipperFromList . map read . lines <$> readFile "input.txt"
  timed $ print $ solve (+1) zipper
  -- this works if you compile the program
  -- but blows the stack in ghci
  timed $ print $ solve part2Func zipper

--------------------------------------------------------------------------------

whileM :: IO Bool -> IO () -> IO ()
whileM condition action = IO go
  where
    go world0 = case condition of
      IO condAction -> case condAction world0 of
        (# world1, cond #) ->
          case cond of
            False -> (# world1, () #)
            True  -> case action of
              IO actionAction -> case actionAction world1 of
                (# world2, _ #) -> go world2

solve' :: V.IOVector Int64 -> Int64 -> (Int64 -> Int64) -> IO Int
solve' vector size f = do
  steps   :: IORef Int64 <- newIORef 0
  currIdx :: IORef Int64 <- newIORef 0
  whileM
    ( do x <- readIORef currIdx
              return $ x >= 0 && x < size
    )
    ( do v <- readIORef currIdx
         let v' = fromIntegral v
         s <- V.read vector v'
         V.write vector v' (f s)
         modifyIORef currIdx (+s)
         modifyIORef steps   (+1)
    )

  fromIntegral <$> readIORef steps

main2 :: IO ()
main2 = do
  input :: [Int64] <- map read . lines <$> readFile "input.txt"
  let !size = length input
      !size64 = fromIntegral size :: Int64
  vector :: V.IOVector Int64 <- V.unsafeNew size
  forM_ (zip [0..] input) $ uncurry (V.write vector)
  timed $ print =<< solve' vector size64 (+1)
  -- this is wrong somehow
  timed $ print =<< solve' vector size64 part2Func

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "build/fold"
  timed main1

  putStrLn ""

  putStrLn "IO MVector"
  timed main2

{-
build/fold
376976
Computation time: 1.950 sec
29227751
Computation time: 14.747 sec
Computation time: 16.698 sec

IO MVector
376976
Computation time: 0.553 sec
739
Computation time: 0.001 sec
Computation time: 0.556 sec
-}
