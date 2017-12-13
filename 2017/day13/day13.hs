#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE TupleSections, LambdaCase, PatternGuards, BangPatterns, DeriveFunctor #-}
import Utils
import Debug.Trace

data Dir = UP | DOWN deriving Show

newtype Scanner = Scanner (Int, Int, Dir)  deriving Show
type FirewallState = [(Int, Maybe Scanner)]

stepScanner :: Scanner -> Scanner
stepScanner (Scanner (i, d, UP))   = Scanner (i+1, d, if (i+1) == d-1 then DOWN else UP)
stepScanner (Scanner (i, d, DOWN)) = Scanner (i-1, d, if (i-1) == 0 then UP else DOWN)


solve1 :: (Int, [FirewallState]) -> Int
solve1 = countCaught . (\(n, steps) -> zipWith (!!) steps [0..n])

countCaught :: FirewallState -> Int
countCaught = getSum . foldMap go
  where
    go (l, Just (Scanner (d,0,_))) = Sum (l*d)
    go _ = mempty

solve2 :: (Int, [FirewallState]) -> Int
solve2 (n, steps) = fst . head . dropWhile ((/= 0) . snd)
                  . map (fmap (countCaught . flip (zipWith (!!)) [0..n]))
                  . zip [0..]
                  . tails
                  $ steps

generate :: String -> (Int, [FirewallState])
generate = fmap (iterate stepAll)
         . initScanner 0
         . map parseScannerDepth
         . lines
  where
    parseScannerDepth xs
      | [l,d] <- words xs = (read (init l), read d)
      | otherwise         = error "BOOM"

    -- returns max index as well as a filled list
    initScanner n [] = (n-1,[])
    initScanner n ((l,d):xs)
      | n == l    = ((l, Just (Scanner (0,d, UP))) :)
                    <$> initScanner (n+1) xs
      | otherwise = ((n, Nothing) :)
                    <$> initScanner (n+1) ((l,d):xs)

    stepAll = fmap (fmap (fmap stepScanner))

main :: IO ()
main = readFile "input.txt" >>= print  . solve1{- (solve1 &&& solve2) -}. generate
