module Lib
    ( star1, star2
    ) where

inputCode :: [Int]
inputCode = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0]

prepareCode :: Int -> Int -> [Int]
prepareCode noun verb =
  (take 1 inputCode) ++ [noun, verb] ++ (drop 3 inputCode)

run :: [Int] -> Int -> Int
run code opcodePosition
    | opcode == 1 = do
      run ((take outputPosition code) ++ [input1 + input2] ++ (drop (outputPosition + 1) code)) (opcodePosition + 4)
    | opcode == 2 = do
      run ((take outputPosition code) ++ [input1 * input2] ++ (drop (outputPosition + 1) code)) (opcodePosition + 4)
    | opcode == 99 = code !! 0
    where opcode = code !! opcodePosition
          input1 = code !! (code !! (opcodePosition + 1))
          input2 = code !! (code !! (opcodePosition + 2))
          outputPosition = code !! (opcodePosition + 3)

execute :: [Int] -> Int
execute code = run code 0

star1 :: IO ()
star1 = do
  let code = prepareCode 12 2
  print (execute code)

star2 :: IO ()
star2 = do
  print . head $ [ 100 * noun + verb | noun <- [0..99], verb <- [0..99], let code = prepareCode noun verb, (execute code) == 19690720]
