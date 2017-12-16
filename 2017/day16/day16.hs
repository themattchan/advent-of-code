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

type Prog = Char
data Move = Spin Int | Exchange Int Int | Partner Prog Prog
  deriving Show

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i e = uncurry (++) . fmap ((e:) . tail) . splitAt i

parseMove :: Parser Move
parseMove = (pS <|> pX <|> pP) <* spaces where
  pS = Spin <$> (char 's' *> num)
  pX = Exchange <$> (char 'x' *> num) <*> (char '/' *> num)
  pP = Partner <$> (char 'p' *> take1) <*> (char '/' *> take1)

solve :: [String] -> Maybe String
solve = foldM go ['a'..'p'] where
  go st c = runParser parseMove c >>= doMove st

doMove st move =
    case move of
      -- list has constant length 15
      Spin i -> return . take 16 . drop (16-(i `mod` 16)) . cycle $ st
      Exchange i j ->
        return $ replaceAt i (st!!j) $ replaceAt j (st!!i) $ st
      Partner x y -> do
        xi <- elemIndex x st
        yi <- elemIndex y st
        return $ replaceAt xi y $ replaceAt yi x $ st

progToInt :: Prog -> Word8
progToInt p = fromIntegral $ ord p - 97
{-# INLINE progToInt #-}

solve2 :: [String] -> IO B.ByteString
solve2 moves = do
  let !moves' = mapMaybe (runParser parseMove) moves

  state :: Ptr Word8 <- mallocBytes 16

  let peekSt :: Int -> IO Word8
      peekSt i = peek (state `plusPtr` i)
      pokeSt :: Int -> Word8 -> IO ()
      pokeSt i e = poke (state `plusPtr` i) e

  forM_ [0..15] $ \offset ->
    -- store char as numbers: a=0, b=1... etc
    pokeSt offset (fromIntegral offset)

  let findIndex c =
            let go i = do
                 v <- peekSt i
                 if v == c then return i else go (i+1)
            in go 0

  let oneRound = forM moves' $ \move -> do
            case move of
              -- list has constant length 15
              Spin i -> do
                 end <- peekArray i (state `plusPtr` (16-i))
                 beg <- peekArray (16-i) state
                 pokeArray state end
                 pokeArray (state `plusPtr` i) beg

--               return . take 16 . drop (16-(i `mod` 16)) . cycle $ st
              Exchange i j -> do
                vi <- peekSt i
                vj <- peekSt j
                pokeSt i vj
                pokeSt j vi
--                return $ replaceAt i (st!!j) $ replaceAt j (st!!i) $ st
              Partner x y -> do
                let x' = progToInt x
                let y' = progToInt y
                xi <- findIndex x'
                yi <- findIndex y'
                pokeSt xi y'
                pokeSt yi x'

  replicateM 1000000000 oneRound
  stateFP <- newForeignPtr_ state
  return $ BI.PS stateFP 0 16

        -- xi <- elemIndex x st
        -- yi <- elemIndex y st
--        return $ replaceAt xi y $ replaceAt yi x $ st

  --     go !n !st
  --       | n == 0 = st
  --       | otherwise = traceShow n $  go (n-1) (fromJust (foldM doMove st moves'))
  -- in do
--    go 1000000000 ['a'..'p']

    -- foldl' (\st ms -> foldl' (\st' m -> st' >>= \s -> doMove s m) st ms )
    --           (Just ['a'..'p']) (replicate 1000000000 moves')

main :: IO ()
main = do
  cs <- splitOn "," <$> readFile "input.txt"
  print $ solve cs
  print =<< solve2 cs
