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

evaluate :: Command -> Program -> IO Program
evaluate Increment (Program (index, array)) = return (Program (index, Map.insertWith (+) index 1 array))
evaluate Decrement (Program (index, array)) = return (Program (index, Map.insertWith (-) index 1 array))
evaluate ShiftLeft (Program (index, array)) = return (Program (index - 1, array))
evaluate ShiftRight (Program (index, array)) = return (Program (index + 1, array))