#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE LambdaCase #-}
import Utils
import Data.Functor.Compose
import Data.Bits
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import qualified Data.Set as S

type End = String
type Input = Either String Int
type Output = String
data Gate = Pure Input
          | Unop (Int -> Int) Input
          | Binop (Int -> Int -> Int) Input Input
data Wire = Wire Gate Output

type Circuit a = State (M.Map String Int) a

getVal :: String -> Circuit Int
getVal v = gets (fromMaybe (error v) . M.lookup v)

runCircuit :: [Wire] -> Circuit ()
runCircuit = foldM (const go) ()
  where
    getIn (Left v) = gets (fromJust . (M.lookup v))
    getIn (Right n) = pure n

    go (Wire (Pure x) o) = getIn x >>= modify . M.insert o
    go (Wire (Unop op x) o) = getIn x >>= modify . M.insert o . op
    go (Wire (Binop op x y) o) = op <$> getIn x <*> getIn y >>= modify . M.insert o

toposort :: [Wire] -> [Wire]
toposort wires = go starts rest
  where
    (starts, rest) = partition
      (\case
          (Wire (Pure (Right _)) _) -> True
          _ -> False
      ) wires

    inscope = foldMap (\(Wire _ out) -> [out])

    dependson (Wire (Pure x) _ ) = lefts [x]
    dependson (Wire (Unop _ x) _ ) = lefts [x]
    dependson (Wire (Binop _ x y) _ ) = lefts [x,y]

    -- can produce output if all the inputs are in scope.
    canproduce depends inscope =
      depends `intersect` inscope == depends

    go sorted [] = sorted
    go sorted unsorted =
      let
        inscopes = inscope sorted
        (sorted1, unsorted1) = partition
          (\wire -> canproduce (dependson wire) inscopes)
            unsorted
      in
        go (sorted ++ sorted1) unsorted1

parseLine :: Parser Wire
parseLine = do
    gate <- pBinop <|> pUnop <|> pLit
    string " -> "
    out <- pVar
    return $ Wire gate out
  where
    pVar = alpha
    pInput = (Right <$> num) <|> (Left <$>pVar)

    pOp :: Parser (Int -> Int -> Int)
    pOp =  literal "AND"     (.&.)
       <|> literal "OR"      (.|.)
       <|> literal "LSHIFT"  shiftL
       <|> literal "RSHIFT"  shiftR

    pBinop, pUnop, pLit :: Parser Gate
    pBinop = do
      v1 <- pInput
      spaces
      op <- pOp
      spaces
      v2 <- pInput
      return $ Binop op v1 v2

    pUnop = do
      string "NOT"
      spaces
      val <- pInput
      return $ Unop complement val

    pLit = Pure <$> pInput

solve1 :: [String] -> Int
solve1 = flip evalState mempty
       . (>> getVal "a")
       . runCircuit
       . toposort
       . mapMaybe (runParser parseLine)

main :: IO  ()
main = do
  instrs <- lines <$> readFile "input.txt"
  print $ solve1 instrs
