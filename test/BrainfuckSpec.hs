module BrainfuckSpec where

import Data.Map
import Test.Hspec

import Brainfuck

spec :: Spec
spec = describe "evaluate" $ do
    it "increments the pointer when ShiftRight is used" $
        evaluate ShiftRight startProgram  `shouldBe` (1,Data.Map.fromList [(x, 0) | x <- [0..9]])