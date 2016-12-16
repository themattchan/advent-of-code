#! /usr/bin/env runhaskell -i../
{-# LANGUAGE RecordWildCards #-}
import Utils

data RawEntry = Entry
  { encName :: String
  , sid     :: Int
  , cksum   :: String
  } deriving Show

parseInp = flip sepBy spaces $
  Entry <$> (concat <$> many (alpha <* (char '-')))
        <*> num
        <*> (between (char '[') (char ']') alpha)

isReal Entry{..}
  = (== cksum)
  . map fst
  . take 5
  . sortBy cmp
  . map (head &&& length)
  . group
  . sort
  $ encName
  where
    cmp x y = c
      where
        c1 = comparing snd y x
        c | c1 == EQ  = comparing fst x y
          | otherwise = c1

main
  = runParser parseInp
  <$> readFile "input.txt"
  >>= print
  . fmap ( sum
         . map sid
         . filter isReal
         )
