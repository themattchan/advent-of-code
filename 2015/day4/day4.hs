#! /usr/bin/env runhaskell -i../../
import Utils
import Data.Hash.MD5
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Bits
import Data.Word

input :: String
input = "iwrupvqb"

abcdA :: ABCD -> Word32
abcdA (ABCD (a,_,_,_)) = a

prefixFiveZeros :: String -> Bool
prefixFiveZeros = (== "00000") . take 5

prefixSixZeros :: String -> Bool
prefixSixZeros = (== "000000") . take 6

chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

pmconcat :: (NFData a, Monoid a) => Int -> [a] -> a
pmconcat cs = go where
  go [x] = x
  go xs  = go . withStrategy (parList rdeepseq) . map mconcat $ chunks cs xs

solve :: (String -> Bool) -> Int -> String -> Int
solve prefixPred start input = go [start..]
  where
    bufsize = 10000
    chunksize = 200

    go xs = case a' of
              First Nothing -> go bs
              First (Just x) -> x
      where
        a' = force $ pmconcat chunksize (map find as)
        as = take bufsize xs
        bs = drop bufsize xs

    find i
      | p = First (Just i)
      | otherwise = mempty
      where
        p = prefixPred
          . md5s . Str
          . (input ++)
          $ show i

main = do
  let fiveZeros = solve prefixFiveZeros 0 input
  print $ fiveZeros
  print $ solve prefixSixZeros fiveZeros  input
