#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns,
DeriveFunctor, ScopedTypeVariables #-}
import Utils
import Data.List.Split
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Word8
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

type Prog = Char
data Move a = Spin Int | Exchange Int Int | Partner a a
  deriving (Show, Functor)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i e = uncurry (++) . fmap ((e:) . tail) . splitAt i

parseMove :: Parser (Move Prog)
parseMove = (pS <|> pX <|> pP) <* spaces where
  pS = Spin <$> (char 's' *> num)
  pX = Exchange <$> (char 'x' *> num) <*> (char '/' *> num)
  pP = Partner <$> (char 'p' *> take1) <*> (char '/' *> take1)

solve :: [String] -> Maybe String
solve = foldM go ['a'..'p'] where
  go st c = do
    move <- runParser parseMove c
    case move of
      -- list has constant length 15
      Spin i ->
        return . take 16 . drop (16-(i `mod` 16)) . cycle $ st
      Exchange i j ->
        return $ replaceAt i (st!!j) $ replaceAt j (st!!i) $ st
      Partner x y -> do
        xi <- elemIndex x st
        yi <- elemIndex y st
        return $ replaceAt xi y $ replaceAt yi x $ st

solve2 :: [String] -> IO B.ByteString
solve2 = run . mapMaybe (fmap (fmap progToW8) . runParser parseMove)
  where
    progToW8 p = fromIntegral $ ord p - 97

    run moves = do
       state :: Ptr Word8 <- mallocBytes 16
       tmp   :: Ptr Word8 <- mallocBytes 16

       let peekSt :: Int -> IO Word8
           peekSt i = peek (state `plusPtr` i)
           pokeSt :: Int -> Word8 -> IO ()
           pokeSt i e = poke (state `plusPtr` i) e

       for_ [0..15] $ \offset ->
         -- store char as numbers: a=0, b=1... etc
          pokeSt offset (fromIntegral offset)

       let findIndex c =
             let go i = do
                  v <- peekSt i
                  if v == c then return i else go (i+1)
             in go 0

       replicateM_ 1000000000 $
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
       return $ B.map (+97) $ BI.PS stateFP 0 16

main :: IO ()
main = do
  cs <- splitOn "," <$> readFile "input.txt"
  print $ solve cs
  print =<< solve2 cs
