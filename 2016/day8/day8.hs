#! /usr/bin/env runhaskell -i../
import Utils
import Data.Bits
import Data.List
import Data.Int

data Command = Rect Int Int
             | Rotate Sel Int Int

data Sel = Row | Col

parseInput = sepBy (parseRect <|> parseRotate) spaces
  where
    parseRect = Rect
             <$> (string "rect" *> spaces *> num)
             <*> (char 'x' *> num)

    parseRotate = Rotate
               <$> (string "rotate" *> spaces *>
                     ((literal "row"    Row) <|>
                      (literal "column" Col)))
               <*> (spaces *> eat 2 *> num)
               <*> (spaces *> string "by" *> spaces *> num)

-- 50 cols x 6 rows
newtype Grid = Grid [Int64]

instance Show Grid where
  show (Grid g) = map p $ unlines $ map showBin g
    where
      p '0' = '_'
      p '1' = '#'
      p  c  =  c

blankGrid :: Grid
blankGrid = Grid (replicate 6 zeroBits)

countGrid :: Grid -> Int
countGrid (Grid g) = sum $ map popCount g

-- 'n' 1's followed by 0's
mask :: Int -> Int64
mask n = bit 63 `shiftR` (n-1)

setGrid (Grid g) m n = Grid g'
  where
    setRow r = r .|. (mask m)
    g' = map setRow a ++ b
      where
        (a,b) = splitAt n g

rotateRow (Grid g) i n = Grid (a ++ b' : bs)
  where
    (a,(b:bs)) = splitAt i g
    b' = rotateRow1 b n

rotateRow1 :: Int64 -> Int -> Int64
rotateRow1 b n = b' where
    topMask = mask (50 - n)
    botMask = ((mask (n+1)) `clearBit` 63) `shiftR` (50 - n -1)
    b' =  ((((b .&. topMask) `shiftR` 1) `clearBit` 63) `shiftR` (n-1))
      .|. ((b .&. botMask) `shiftL` (50 - n))

rotateCol (Grid g) i n = Grid g'
  where
    -- Word64 index: 63 .. 14 .. 0
    -- Grid   index: 0  .. 49
    i' = 63 - i
    n' = length col - n `mod` length col
    col = map (flip testBit i') g
    rcol = take (length col) (drop n' (cycle col))
    g' = zipWith switchBit rcol g
    switchBit True w = setBit w i'
    switchBit False w = clearBit w i'

-- test1 = Grid ([ bit 63, bit 63] ++ replicate 4 zeroBits)

run :: [Command] -> Grid
run = foldl' interp blankGrid
  where
    interp g (Rect m n)       = setGrid   g m n
    interp g (Rotate Row i n) = rotateRow g i n
    interp g (Rotate Col i n) = rotateCol g i n

main = do
  i <- runParser parseInput <$> readFile "input.txt"
  case i of
    Nothing -> undefined
    Just cmds -> do
      print $ countGrid (run cmds)
      putStrLn (show (run cmds))
