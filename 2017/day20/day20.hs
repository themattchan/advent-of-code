#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
import Utils
import qualified Data.Vector.Unboxed as V
import qualified Data.Semigroup as S
import Data.Int

newtype V3 a = V3 [a] --(V.Vector a) --deriving (Functor, Foldable, Traversable)
  deriving (Show, Eq)
mkV3 :: V.Unbox a => [a] -> V3 a
mkV3 xs = V3 xs -- $ V.fromList xs

instance (Num a, V.Unbox a) => S.Semigroup (V3 a) where
  V3 xs <> V3 ys = V3 (zipWith (+) xs ys)
  {-# INLINEABLE (<>) #-}
  {-# SPECIALISE (<>) :: V3 Int64 -> V3 Int64 -> V3 Int64 #-}

distV3 :: (Num a, V.Unbox a)=> V3 a -> a
distV3 (V3 v) = sum $ map abs v
{-# SPECIALISE distV3 :: V3 Int64 -> Int64 #-}

data Particle = Particle { particleP :: V3 Int
                         , particleV :: V3 Int
                         , particleA :: V3 Int
                         } deriving Show

parser :: Parser [Particle]
parser = many (particle <* spaces)
  where
    particle = do
      Particle
      <$> (string "p=" *> pvec <* string ", ")
      <*> (string "v=" *> pvec <* string ", ")
      <*> (string "a=" *> pvec)
    pvec = mkV3  <$> between (char '<') (char '>') (sepBy num (char ','))

-- p0 = <x0,y0,z0>
-- v = <vx,vy,vz>
-- a = <ax,ay,az>
-- p(t) = <x0 + vx + ax*t, y0+vy+ay*t, z0+vz+az*t>
-- position over all time
-- integrate p dt = <x0t + vxt + ax/2 *t^2,...>
-- subst x0,y0,z0

-- p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
-- p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>
-- (2*6 + -1/2 t^2) +0 +0
-- 12 -1/2t^2
-- (4*0+ -8/2 t^2)
-- 0 -4t^2

minParticle :: [Particle] -> Int
minParticle = fst . minimumBy cmp . zip [0..]
  where
    accel = distV3 . particleA
    init = distV3 . particleP
    cmp (_,p1) (_,p2) = case compare (accel p1) (accel p2) of
      EQ -> compare (init p1) (init p2)
      x -> x

stepParticle :: Particle -> Particle
stepParticle p@Particle{..} = p { particleP = particleP S.<> particleV S.<> particleA
                                , particleV = particleV S.<> particleA
                                }

collideUntilDone :: [Particle] -> Int
collideUntilDone xs
  | done = length xs
  | otherwise = collideUntilDone xs'
  where
    colls = map head . filter ((>1) . length) . groupBy (==) . map particleP $ xs
    xs' = map stepParticle . filter (not . (`elem` colls) . particleP) $ xs

    ordered cmp xs = all (`elem` [LT,EQ]) $ zipWith cmp xs (tail xs)

    done = let sorted = sortOn (distV3 . particleP) xs
           in ordered (comparing (distV3 . particleV)) sorted && ordered (comparing (distV3 . particleA)) sorted


main :: IO ()
main = do
  Just ps <- runParser parser <$> readFile "input.txt"
--  print ps
  print $ minParticle ps
  print $ collideUntilDone ps
