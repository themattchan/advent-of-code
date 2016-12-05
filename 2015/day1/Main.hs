step f '(' = f + 1
step f ')' = f - 1

firstNeg = snd . foldl go (0, 0)
  where go (f,s) c = if f > -1 then (step f c, s+1) else (f,s)

main = do
  ps <- readFile "input.txt"
  print $ foldl step 0 ps
  print $ firstNeg ps
