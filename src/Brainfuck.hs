module Brainfuck
    ( Program(..),
      startProgram,
      Command(..),
      evaluate,
    ) where

import Data.Map(Map)
import qualified Data.Map as Map

newtype Program = Program (Int, Map Int Int) deriving (Show, Eq)

startProgram :: Program
startProgram = Program (0, Map.fromList [(x, 0) | x <- [0..9]])

data Command = Increment | Decrement | ShiftLeft | ShiftRight |
               Output | Input | OpenLoop | CloseLoop

getIntInput :: IO Int 
getIntInput = do 
                  putStrLn "Enter a number"
                  a <- getLine 
                  return (read a :: Int)
                  

evaluate :: Command -> Program -> IO Program
evaluate Increment (Program (index, array)) = return (Program (index, Map.insertWith (+) index 1 array))
evaluate Decrement (Program (index, array)) = return (Program (index, Map.insertWith (+) index (-1) array))
evaluate ShiftLeft (Program (index, array)) = return (Program (index - 1, array))
evaluate ShiftRight (Program (index, array)) = return (Program (index + 1, array))
evaluate Output (Program (index, array)) = do (putChar . toEnum) $ (Map.!) array index
                                              return (Program (index, array)) 
evaluate Input (Program (index, array)) = do a <- getIntInput
                                             return (Program (index, Map.insert index a array))
                                             