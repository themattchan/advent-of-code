{-# LANGUAGE BangPatterns #-}
import Utils

input :: Int
input = 681901

gen :: [Int]
gen = 3 : 7 : go 0 1 2
  where
    go !i !j !l =
      let ix = (gen !! i)
          jx = (gen !! j)
          x = ix + jx
          (x1, x2) = divMod x 10
          f = if x1 == 0 then (x2 :) else ((x1 :) . (x2 :))
          l' = l + if x1 == 0 then 1 else 2
          i' = (i + 1 + ix) `mod` l'
          j' = (j + 1 + jx) `mod` l'
      in f (go i' j' l')


tests = [ take 10 (drop 9 gen) == [5,1,5,8,9,1,6,7,7,9]
        , take 10 (drop 5 gen) == [0,1,2,4,5,1,5,8,9,1]
        , take 10 (drop 18 gen) == [9,2,5,1,0,7,1,0,8,5]
        , take 10 (drop 2018 gen) == [5,9,4,1,4,2,9,8,8,2]
        ]

main = do
  putStrLn $ foldMap show $ take 10 (drop input gen)
