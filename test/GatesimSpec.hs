module GatesimSpec (spec) where

import Test.Hspec
import Control.Monad
import Gatesim.Gatesim

inputs :: [GateArray]
inputs = map fromList
    [
        toInput [4, 5, 2] ++ [ Sum 0 2, Exp 1 2, Prod 3 4 ],
        toInput [6, 3, 7] ++ [ Sum 0 2, Exp 1 2, Prod 3 4 ],
        toInput [0, 3, 9] ++ [ Sleep 0, Exp 1 2, Prod 3 4 ],
        toInput [100] ++ [ Sleep 0 ]
    ]

outputs :: [Int]
outputs = [150, 28431, 0, 100]

tests :: [(GateArray, Int)]
tests = zip inputs $ map (`bseq` 0) inputs

testSeq :: (GateArray -> Int) -> Spec
testSeq f = forM_ (zip inputs outputs) (\(i, o) -> it (show o) $ f i `shouldBe` o)

testFun :: (GateArray -> Int) -> Spec
testFun f = forM_ tests (\(i, o) -> it (show o) $ f i `shouldBe` o)

spec :: Spec
spec = do
    describe "seq" $ testSeq (`bseq` 0)
    describe "strategies" $ testFun (`bstrat` 0)
    describe "repa" $ testFun (`brepa` 0)
    describe "monad par" $ testFun (`bmpar` 0)
