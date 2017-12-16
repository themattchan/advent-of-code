#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns,
DeriveFunctor, ScopedTypeVariables, KindSignatures,DataKinds
#-}
import Utils
--import Data.List.Split
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Word8
-- import Foreign.Marshal.Alloc
-- import Foreign.Ptr
-- import Foreign.ForeignPtr
-- import Foreign.Storable
-- import Foreign.Marshal.Array
-- import Foreign.Marshal.Utils
import qualified Data.Map as M
import GHC.TypeLits
import Data.Proxy
import qualified Data.Semigroup as S


type Prog = Char
data Move = Spin Int | Exchange Int Int | Partner Prog Prog
  deriving Show

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i e = uncurry (++) . fmap ((e:) . tail) . splitAt i

parseMove :: Parser [Move]
parseMove = sepBy ((pS <|> pX <|> pP) <* spaces) (char ',') where
  pS = Spin <$> (char 's' *> num)
  pX = Exchange <$> (char 'x' *> num) <*> (char '/' *> num)
  pP = Partner <$> (char 'p' *> take1) <*> (char '/' *> take1)

solve :: [Move] -> String
solve = foldl go ['a'..'p'] where
  go st = \case
      -- list has constant length 15
      Spin i ->
        take 16 . drop (16-(i `mod` 16)) . cycle $ st
      Exchange i j ->
        replaceAt i (st!!j) $ replaceAt j (st!!i) $ st
      Partner x y -> fromJust $ do
        xi <- elemIndex x st
        yi <- elemIndex y st
        return $ replaceAt xi y $ replaceAt yi x $ st

{-
solve2 :: [String] -> IO B.ByteString
solve2 = run . mapMaybe (fmap (fmap progToW8) . runParser parseMove)
  where
    progToW8 p = fromIntegral $ ord p - 97

    run moves = do
       countRepeats <- newIORef mempty
       state :: Ptr Word8 <- mallocBytes 16
       tmp   :: Ptr Word8 <- mallocBytes 16

       let peekSt :: Int -> IO Word8
           peekSt i = peek (state `plusPtr` i)
           pokeSt :: Int -> Word8 -> IO ()
           pokeSt i e = poke (state `plusPtr` i) e

           makeBS fp = B.map (+97) $ BI.PS fp 0 16

       for_ [0..15] $ \offset ->
         -- store char as numbers: a=0, b=1... etc
          pokeSt offset (fromIntegral offset)

       let findIndex c =
             let go i = do
                  v <- peekSt i
                  if v == c then return i else go (i+1)
             in go 0

       replicateM_ 1000000000 $ do
         for_ moves $ \case
           Spin i -> do
             copyArray tmp (state `plusPtr` (16-i)) i
             moveArray (state `plusPtr` i) state (16-i)
             copyArray state tmp i

           Exchange i j -> do
             vi <- peekSt i
             vj <- peekSt j
             pokeSt i vj
             pokeSt j vi

           Partner x y -> do
             xi <- findIndex x
             yi <- findIndex y
             pokeSt xi y
             pokeSt yi x

       stateFP <- newForeignPtr_ state
       return $ makeBS stateFP
-}

-- this somehow forms a permutation group
-- the input should be munged into a SINGLE permutation group
-- then apply the group transform a billion times
-- BEFORE calculating anything

-- store the image of [0..n-1] under permutation
data Permutation (n :: Nat) = Permutation [Int] deriving Show

instance KnownNat n => S.Semigroup (Permutation n) where
  Permutation p1 <> Permutation p2 = Permutation [p2 !! i | i <- p1]

solve2 :: [Move] -> String
solve2 = unPerm . S.stimes 1000000000 . mkPerm . filter notPartner
  where
    -- label swapping is a transposition. in an even number of repetitions,
    -- this is idempotent.
    notPartner (Partner _ _) = False
    notPartner _ = True

    mkPerm moves = Permutation [ord c - ord 'a'  | c <- solve moves] :: Permutation 16

    unPerm (Permutation out) = [chr (i + ord 'a') | i <- out]

main :: IO ()
main = do
  Just moves <- runParser parseMove <$> readFile "input.txt"
  putStrLn $ solve moves
  putStrLn $ solve2 moves
