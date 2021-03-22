module Brainfuck
    ( Program(..),
      startProgram,
      Command(..),
      evaluate,
    ) where

import Data.Map(Map)
import qualified Data.Map as Map

data Program = Program {instruction :: Int, loopIndices :: [Int], index :: Int, array :: Map Int Int } deriving (Show, Eq)

startProgram :: Program
startProgram = Program {instruction = 0, loopIndices = [], index = 0, array = Map.fromList [(x, 0) | x <- [0..9]]}

data Command = Increment | Decrement | ShiftLeft | ShiftRight |
               Output | Input | OpenLoop | CloseLoop

getIntInput :: IO Int 
getIntInput = do 
                  putStrLn "Enter a number"
                  a <- getLine 
                  return (read a :: Int)
                  

evaluate :: Command -> Program -> IO Program
evaluate Increment program = return program { array = Map.insertWith (+) (index program) 1 (array program) }
evaluate Decrement program = return program { array = Map.insertWith (+) (index program) (-1) (array program) }
evaluate ShiftLeft program = return program { index = index program - 1 }
evaluate ShiftRight program = return program { index = index program + 1 } 

evaluate Output program = do 
    (putChar . toEnum) $ array program Map.! index program
    return program

evaluate Input program = do
    a <- getIntInput
    return program { array = Map.insert (index program) a (array program) }

evaluate OpenLoop program = return program { loopIndices = instruction program : loopIndices program }

evaluate CloseLoop program 
    | array program Map.! index program == 0    = return program {loopIndices = drop 1 $ loopIndices program }
    | otherwise    = return program {instruction = head $ loopIndices program, loopIndices = drop 1 $ loopIndices program }       