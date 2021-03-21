module BrainfuckSpec where

import Data.Map
import Test.Hspec
--import Control.Monad.IO

import Brainfuck

spec :: Spec
spec = describe "evaluate" $ do
    it "increases the value stored at the index when Increment is used" $ do
        Program {array=arr, index=ind} <- evaluate Increment startProgram
        arr ! ind `shouldBe` 1

    it "decreases the value stored at the index when Decrement is used" $ do
        Program {array=arr, index=ind} <- evaluate Decrement $ Program 0 1 (Data.Map.fromList [(x, 2) | x <- [0..9]])
        arr ! ind `shouldBe` 1

    it "increments the pointer when ShiftRight is used" $ do
        result <- evaluate ShiftRight startProgram
        result `shouldBe` Program 0 1 (Data.Map.fromList [(x, 0) | x <- [0..9]])

    it "decrements the pointer when ShiftLeft is used" $ do
        a <- evaluate ShiftRight startProgram
        result <- evaluate ShiftLeft a
        result `shouldBe` startProgram