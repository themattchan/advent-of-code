#! /usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings, ExistentialQuantification, Rank2Types #-}

import Prelude hiding (FilePath)
import Turtle
import Data.Time
import Data.Text (unpack)

type Puzzle = (FilePath, FilePath)
type PuzzleCommand = forall io. MonadIO io => Puzzle -> io ()

parseOpts :: MonadIO io => Int -> Parser (io ())
parseOpts year
  =  newPuzzle
         <$> subcommand "new" "Make a new puzzle"  parsePuzzle
 <|> runPuzzle
         <$> subcommand "run" "Run puzzle in ghci" parsePuzzle
 <|> compilePuzzle
         <$> subcommand "compile" "Compile puzzle" parsePuzzle
  where
    yearDesc = fromString $ "The year (default: "++show year++")"
    parsePuzzle = puzzlePath year
                  <$> (optInt "year" 'y' yearDesc <|> pure year)
                  <*> argInt "day" "The day"

fpath :: Text -> FilePath
fpath = fromString . unpack

puzzlePath :: Int -> Int -> Int -> Puzzle
puzzlePath maxYr yr day
  | not $ 2015 <= yr  && yr  <= maxYr = error $ "Invalid year: " ++ show yr
  | not $ 1    <= day && day <= 25    = error $ "Invalid day: " ++ show day
  | otherwise =
    let dir  = fpath $ format (d%"/day"%d) yr day
        file = fpath $ format ("day"%d%".hs") day
    in (dir, file)

newPuzzle :: PuzzleCommand
newPuzzle (dir, file) = do
  mkdir dir
  cp "boilerplate.hs" (dir</>file)

runPuzzle :: PuzzleCommand
runPuzzle (dir, file) = do
  cd dir
  stdout (inshell (format ("./"%fp) file) mempty)

compilePuzzle :: PuzzleCommand
compilePuzzle (dir, file) = do
  cd dir
  stdout (inshell (format ("stack ghc  -- "%fp% "  -O2 -i ../../Utils.hs") $ file) mempty)

main :: IO ()
main = do
  (y,m,d) <- toGregorian . utctDay <$> date
  let y' = if m < 12 then y-1 else y -- not christmas yet!
  join $ options "Advent of Code" (parseOpts (fromIntegral y'))
