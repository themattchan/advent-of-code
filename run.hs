#! /usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle
import Data.Time
import Data.Text (unpack)

type Puzzle = (FilePath, FilePath)

data Runner = New Puzzle | Run Puzzle | Compile Puzzle

parseOpts :: Int -> Parser Runner
parseOpts year
  =  New     <$> subcommand "new" "Make a new puzzle"  parsePuzzle
 <|> Run     <$> subcommand "run" "Run puzzle in ghci" parsePuzzle
 <|> Compile <$> subcommand "compile" "Compile puzzle" parsePuzzle
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

main :: IO ()
main = do
  (y,m,d) <- toGregorian . utctDay <$> date
  let y' = if m < 12 then y-1 else y -- not christmas yet!
  runner <- options "Advent of Code" (parseOpts (fromIntegral y'))
  case runner of
    New (dir, file) -> do
      mkdir dir
      cp "boilerplate.hs" (dir</>file)

    Run (dir, file) -> do
      cd dir
      stdout (inshell (format ("./"%fp) file) mempty)

    Compile (dir, file) -> do
      cd dir
      stdout (inshell (format ("stack ghc  -- "%fp% "  -O2 -i ../../Utils.hs") $ file) mempty)
