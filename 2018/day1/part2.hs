{-# LANGUAGE TypeApplications #-}
import Data.List
import qualified Data.IntSet as IS
import Debug.Trace
main = do
  input <- map (read @Int . filter (/='+')) . lines <$> readFile "input"
  let as = (scanl (+) 0 . cycle $ input)
  print $ take 100 as
  let go seen (x:xs) | x `IS.member` seen = x
                     | otherwise =  -- trace (show x) $
                                  go (x `IS.insert` seen) xs

  print $ go mempty as
  pure ()
