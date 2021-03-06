module Brainfuck
    ( Program,
      startProgram,
      Command(..),
      evaluate,
    ) where

import Data.Map(Map)
import qualified Data.Map as Map

type Program = (Int, Map Int Int)

startProgram :: Program
startProgram = (0, Map.fromList [(x, 0) | x <- [0..9]])

data Command = Increment | Decrement | ShiftLeft | ShiftRight |
               Output | Input | OpenLoop | CloseLoop

evaluate :: Command -> Program -> Program
evaluate Increment (index, array) = (index, Map.insertWith (+) index 1 array)
evaluate Decrement (index, array) = (index, Map.insertWith (-) index 1 array)
evaluate ShiftLeft (index, array) = (index - 1, array)
evaluate ShiftRight (index, array) = (index + 1, array)