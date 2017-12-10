#! /usr/bin/env runhaskell -i../../
import Utils
import Data.Functor.Compose
import Data.Bits
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M

type Circuit a = State (M.Map String Int) a

getVal :: String -> Circuit Int
getVal v = gets (fromMaybe (error v) . M.lookup v)

parseLine :: Parser (Circuit ())
parseLine = do
    val <- pBinop <|> pUnop <|> pLit
    string " -> "
    var <- pVar
    return $ val >>= modify . M.insert var
  where
    pVar = alpha
    pVarVal = getVal <$> pVar

    pOp :: Parser (Int -> Int -> Int)
    pOp =  literal "AND"     (.&.)
       <|> literal "OR"      (.|.)
       <|> literal "LSHIFT"  shiftL
       <|> literal "RSHIFT"  shiftR

    pBinop, pUnop, pLit :: Parser (Circuit Int)
    pBinop = do
      v1 <- pLit <|> pVarVal
      spaces
      op <- pOp
      spaces
      v2 <- pLit <|> pVarVal
      return $ op <$> v1 <*> v2

    pUnop = do
      string "NOT"
      spaces
      val <- pLit <|> pVarVal
      return $ complement <$> val

    pLit = (pure <$> num) <|> pVarVal

solve1 :: [String] -> Maybe Int
solve1 = fmap ( flip evalState mempty
              . (>> getVal "a")
              )
       . getCompose
       . traverse_ (Compose . runParser parseLine)

main :: IO  ()
main = do
  instrs <- lines <$> readFile "input.txt"
  let bad =  filter (not . snd) $ map (fmap (isJust . runParser parseLine) )  $ zip [0..] instrs
  print $ map ((instrs !!) . fst ) bad
  print $ solve1 instrs
