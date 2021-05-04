module Brainfuck
    ( Arr,
      Program(..),
      startProgram,
      Command(..),
      evaluate,
      execute,
      runBrainfuck,
    ) where

import Control.Applicative ( Applicative(liftA2), ZipList (ZipList, getZipList) )
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import System.IO ( openFile, IOMode(ReadMode), hGetContents, hClose )

type Arr = Map Int

data Program = Program {instruction :: Int, loopIndices :: [Int], index :: Int, array :: Arr Int } deriving (Show, Eq)

startProgram :: Program
startProgram = Program {instruction = 0, loopIndices = [], index = 0, array = Map.fromList [(x, 0) | x <- [0..9]]}

data Command = Increment | Decrement | ShiftLeft | ShiftRight |
               Output | Input | OpenLoop | CloseLoop | None deriving (Show)

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
execute com program = 
    if instruction program < Map.size com
        then do 
            newProgram <- evaluate (com Map.! instruction program) program
            execute com newProgram {instruction = instruction program + 1}
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

getTokens :: [Char] -> Arr Command
getTokens c = let tokens = map tokenizeChar c
                  tups   =  getZipList $ (,) <$> ZipList [0..] <*> ZipList tokens
              in  Map.fromList tups

runBrainfuck :: String -> IO ()
runBrainfuck source = do 
    contents <- readFile source
    execute (getTokens contents) startProgram
    return ()