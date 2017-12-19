#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils

solve :: String -> Maybe Int
solve = runParser go  where
  go = sum <$> many parser
  parser = marker <|> (take1 *> pure 1)
  marker = do
    (len,rep) <- parens ((,) <$> (num  <* char 'x') <*> num)
    takeN len
    pure $ len * rep

solve2 :: String -> Maybe Int
solve2 = runParser go  where
  go = sum <$> many parser
  parser = marker <|> (take1 *> pure 1)
  marker = do
    (len,rep) <- parens ((,) <$> (num  <* char 'x') <*> num)
    xs <- takeN len
    let Just len' = runParser go xs
    pure $ len' * rep

main :: IO ()
main = readFile "input.txt" >>= print . (solve &&& solve2) . filter (not . isSpace)
