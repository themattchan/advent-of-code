#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE DeriveFunctor, PatternGuards #-}
import Utils
import qualified Data.Map as M
import Data.Semigroup
import Debug.Trace

type Registers = M.Map String (Max Int, Int)

runCommands :: Registers -> String -> Registers
runCommands r l = case words l of
  [rMod, comm, n, "if", rDep, op, m] ->
    let f | comm == "inc" = (+ (read n))
          | otherwise     = subtract (read n)
        g = flip (parseOp op) (read m)

        alterOp
          | g $ maybe 0 snd (M.lookup rDep r)
          = pure . first (uncurry mappend) . assocl . fmap ((Max &&& id) . f) . fromMaybe (pure 0)
          | otherwise
          = id
    in
      M.alter alterOp rMod r

  _ -> error "DIE"

parseOp :: String -> Int -> Int -> Bool
parseOp ">"  = (>)
parseOp "<"  = (<)
parseOp ">=" = (>=)
parseOp "<=" = (<=)
parseOp "==" = (==)
parseOp "!=" = (/=)
parseOp x    = error $ "DIE " ++ x

solve :: [String] -> (Max Int, Max Int)
solve = swap . foldMap (id *** Max) . foldl runCommands mempty

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines
