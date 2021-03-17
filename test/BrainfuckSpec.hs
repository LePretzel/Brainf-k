module BrainfuckSpec where

import Data.Map
import Test.Hspec
--import Control.Monad.IO

import Brainfuck

spec :: Spec
spec = describe "evaluate" $ do
    it "increments the pointer when ShiftRight is used" $ do
        a <- evaluate ShiftRight startProgram
        a `shouldBe` Program (1, Data.Map.fromList [(x, 0) | x <- [0..9]])