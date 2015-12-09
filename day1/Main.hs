main :: IO ()
main = do
  ps <- readFile "input.txt"
  let floor = foldl step 0 ps
  print floor
  where
    step f '(' = f + 1
    step f ')' = f - 1
