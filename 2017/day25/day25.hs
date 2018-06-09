#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import Zipper
import Data.Semigroup
import Debug.Trace

data State = A | B | C | D | E | F

zipLeftInf :: Int -> Zipper Int -> Zipper Int
zipLeftInf 0 z = z
zipLeftInf n (Zipper ([],e,rs)) = Zipper ([],0,(e:rs))
zipLeftInf n (Zipper (l:ls,e,rs)) = zipLeftInf (n-1) (Zipper (ls, l, e:rs))

zipRightInf :: Int -> Zipper Int -> Zipper Int
zipRightInf 0 z = z
zipRightInf n (Zipper (ls,e,[])) = Zipper (e:ls, 0, [])
zipRightInf n (Zipper (ls,e,r:rs)) = zipRightInf (n-1) (Zipper (e:ls, r, rs))

left = zipLeftInf 1
right = zipRightInf 1

blank = zipperFromList [0]

one = modifyFoci (const 1)
zero = modifyFoci (const 0)


machine :: (State, Zipper Int) -> (State, Zipper Int)
machine (s,z) =
  let current = viewFoci z == 0
  in case s of
    A -> if current then (B, right . one $ z) else (C, left . zero $ z)
    B -> if current then (A, left  . one $ z) else (D, left . one  $ z)
    C -> if current then (D, right . one $ z) else (C, right . zero $ z)
    D -> if current then (B, left . zero $ z) else (E, right . zero $ z)
    E -> if current then (C, right . one $ z) else (F, left . one $ z)
    F -> if current then (E, left . one $ z)  else (A, right . one $ z)

doIt = appEndo (stimes reps (Endo machine)) init
  where
    reps = 12656374
    init = (A, blank)

main = foldMap (print . sum . unzipper) doIt
