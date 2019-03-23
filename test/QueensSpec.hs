module QueensSpec (spec) where

import Test.Hspec
import Control.Monad
import Queens.Queens

inputs :: [Int]
inputs = [2, 4..10]

tests :: [(Int, Int)]
tests = zip inputs $ map bseq inputs

testFun :: (Int -> Int) -> Spec
testFun f = forM_ tests (\(i, o) -> it (show i) $ f i `shouldBe` o)

spec :: Spec
spec = do
    describe "seq" $ do
        it "3" $ bseq 3 `shouldBe` 0
        it "4" $ bseq 4 `shouldBe` 2
        it "13" $ bseq 13 `shouldBe` 73712
    describe "strategies" $ testFun bstrat
    describe "repa" $ testFun brepa
    describe "monad par" $ testFun bmpar
