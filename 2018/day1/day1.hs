{-# LANGUAGE TypeApplications #-}
import qualified Data.IntSet as IS
main = do
  input <- map (read @Int . filter (/='+')) . lines <$> readFile "input"

  print (sum input)

  let go seen (x:xs) | x `IS.member` seen = x
                     | otherwise =  -- trace (show x) $
                                  go (x `IS.insert` seen) xs

  print $ go mempty (scanl (+) 0 . cycle $ input)
