#! /usr/bin/env runhaskell -i../../
import Utils
import qualified Data.Map as M
import Data.Semigroup

type Registers = M.Map String (Max Int, Int)

runCommand :: Registers -> String -> Registers
runCommand r l = case words l of
  [rMod, comm, n, "if", rCmp, cmp, m] ->
    let f | comm == "inc" = (+ (read n))
          | otherwise     = subtract (read n)

        g = flip (parseCmp cmp) (read m)

        h | g $ maybe 0 snd (M.lookup rCmp r)
          = pure . first (uncurry mappend) . assocl . fmap ((Max &&& id) . f) . fromMaybe (pure 0)
          | otherwise
          = id
    in
      M.alter h rMod r

  _ -> error $ "DIE " ++ l

parseCmp :: String -> Int -> Int -> Bool
parseCmp ">"  = (>)
parseCmp "<"  = (<)
parseCmp ">=" = (>=)
parseCmp "<=" = (<=)
parseCmp "==" = (==)
parseCmp "!=" = (/=)
parseCmp x    = error $ "DIE " ++ x

solve :: [String] -> (Max Int, Max Int)
solve = swap . foldMap (id *** Max) . foldl runCommand mempty

main :: IO ()
main = readFile "input.txt" >>= timed . print . solve . lines
