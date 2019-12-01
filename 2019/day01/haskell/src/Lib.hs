module Lib
    ( calculate
    ) where

fuel1 :: Integer -> Integer
fuel1 mass =
  if mass < 9
    then 0
    else
      mass `div` 3 - 2

fuel2 :: Integer -> Integer
fuel2 0 = 0
fuel2 mass =
  fuel1 mass + fuel2 (fuel1 mass)

calculate :: IO ()
calculate = do
  contents <- readFile "src/input"
  let masses = map (\xs -> read xs :: Integer) (lines contents)
  let fuels1 = foldl (+) 0 (map fuel1 masses)
  let fuels2 = foldl (+) 0 (map fuel2 masses)
  putStrLn (show fuels1)
  putStrLn (show fuels2)
