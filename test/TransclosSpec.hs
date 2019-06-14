module TransclosSpec (spec) where

import Test.Hspec
import Control.Monad
import Transclos.Transclos

inputs :: [(Int -> [Int], Int, [Int])]
inputs = [(r1 100, 20, [1, 50]), (r2, 200, [1]), (r1 100, 200, [1])]

outputs :: [Bool]
outputs = [True, True, False]

tripUncurry :: (a -> b -> c -> d) -> (a, b, c) -> d
tripUncurry f (a, b, c) = f a b c

tests :: [((Int -> [Int], Int, [Int]), Bool)]
tests = zip inputs outputs -- $ map (tripUncurry bseq) inputs

testSeq :: ((Int -> [Int]) -> Int -> [Int] -> Bool) -> Spec
testSeq f = forM_ (filter snd tests) (\(i, o) -> it (show o) $ tripUncurry f i `shouldBe` o)

testFunOne :: ((Int -> [Int]) -> Int -> [Int] -> Bool) -> ((Int -> [Int], Int, [Int]), Bool) -> Spec
testFunOne f (i, o) = it (show o) $ tripUncurry f i `shouldBe` o

spec :: Spec
spec = do
    describe "seq" $ testSeq bseq
    -- bstrat can't return False, it just loops forever
    describe "strategies" $ forM_ (filter snd tests) (testFunOne bstrat)
    describe "repa" $ forM_ tests (testFunOne brepa)
    describe "monad par" $ forM_ tests (testFunOne bmpar)
