module CoinsSpec (spec) where

import Test.Hspec
import Control.Monad
import Data.List
import Coins.Coins

vals, quants1, quants2, quants3 :: [Int]
vals = [250, 100, 25, 10, 5, 1]
quants1 = [1, 3, 2, 5, 7, 12]           -- small setup
quants2 = [5, 8, 8, 9, 12, 17]           -- std setup
quants3 = [55, 88, 88, 99, 122, 177]  -- large setup

coins1 = zip vals quants1
coins2 = zip vals quants2
coins3 = zip vals quants3

inputs :: [(Int, [(Int, Int)])]
inputs = [(122, coins1), (122, coins2), (122, coins3), (873, coins1)]

tests :: [((Int, [(Int, Int)]), Int)]
tests = zip inputs $ map (uncurry bseq) inputs

testFun :: ((Int, [(Int, Int)]) -> Int) -> Spec
testFun f = forM_ tests (\(i, o) -> it "test" $ f i `shouldBe` o)

spec :: Spec
spec = do
    describe "seq" $
        it "original" $ bseq 873 coins3 `shouldBe` 206940
    describe "strategies" $ testFun $ uncurry bstrat
    describe "repa" $ testFun $ uncurry brepa
    describe "monad par" $ testFun $ uncurry bmpar
