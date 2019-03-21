module QueensSpec (spec) where

import Test.Hspec
import Control.Monad
import Queens

inputs :: [Int]
inputs = [2..10]

tests :: [(Int, Int)]
tests = zip inputs $ map bseq inputs

testFun :: (Int -> Int) -> Spec
testFun f = forM_ tests (\(i, o) -> it (show i) $ f i `shouldBe` o)

spec :: Spec
spec = do
    describe "strategies" $ testFun bstrat
    describe "repa" $ testFun brepa
    describe "monad par" $ testFun bmpar
