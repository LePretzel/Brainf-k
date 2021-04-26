module Brainfuck
    ( Program(..),
      startProgram,
      Command(..),
      evaluate,
      execute,
    ) where

import Control.Monad(when)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as T

type Arr = Map Int

data Program = Program {instruction :: Int, loopIndices :: [Int], index :: Int, array :: Arr Int } deriving (Show, Eq)

startProgram :: Program
startProgram = Program {instruction = 0, loopIndices = [], index = 0, array = Map.fromList [(x, 0) | x <- [0..9]]}

data Command = Increment | Decrement | ShiftLeft | ShiftRight |
               Output | Input | OpenLoop | CloseLoop | None

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

evaluate None program = return program


execute :: Arr Command -> Program -> IO Program 
execute com program = do
    newProgram <- evaluate (com Map.! instruction program) program
    if instruction program < Map.size com
        then execute com newProgram
        else return program


tokenizeChar :: Char -> Command
tokenizeChar c | c == '+' = Increment
               | c == '-' = Decrement
               | c == '>' = ShiftRight
               | c == '<' = ShiftLeft
               | c == ',' = Input
               | c == '.' = Output
               | c == '[' = OpenLoop
               | c == ']' = CloseLoop
               | otherwise = None

getTokens :: [Char] -> [Command]
getTokens = map tokenizeChar