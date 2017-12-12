#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE LambdaCase, DeriveFunctor #-}
import Utils
import Data.Functor.Compose
import Data.Bits
import qualified Data.Map.Lazy as M

type Input = Either String Int

data Gate i = Pure i
            | Unop (Int -> Int) i
            | Binop (Int -> Int -> Int) i i
            deriving Functor

type Circuit = M.Map String (Gate Input)
type Result = M.Map String Int

runCircuit :: Circuit -> Result
runCircuit circuit = result
  where
    result = fmap (evalGate . fmap resolve) circuit

    resolve (Left v)  = result M.! v
    resolve (Right n) = n

    evalGate (Pure x)       = x
    evalGate (Unop op x)    = op x
    evalGate (Binop op x y) = op x y

parseLine :: Parser (String, Gate Input)
parseLine = do
    gate <- pBinop <|> pUnop <|> pLit
    string " -> "
    out <- pVar
    return (out, gate)
  where
    pVar = alpha
    pInput = (Right <$> num) <|> (Left <$> pVar)

    pOp :: Parser (Int -> Int -> Int)
    pOp =  literal "AND"     (.&.)
       <|> literal "OR"      (.|.)
       <|> literal "LSHIFT"  shiftL
       <|> literal "RSHIFT"  shiftR

    pBinop, pUnop, pLit :: Parser (Gate Input)
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

solve :: [String] -> (Int,Int)
solve ss = (part1, part2)
  where
    circuit = M.fromList
            . mapMaybe (runParser parseLine)
            $ ss
    go c = runCircuit c M.! "a"
    part1 = go circuit
    part2 = go (M.insert "b" (Pure (Right part1)) circuit)

main :: IO  ()
main = readFile "input.txt" >>= print . solve . lines
