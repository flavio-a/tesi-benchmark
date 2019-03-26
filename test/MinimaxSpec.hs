module MinimaxSpec (spec) where

import Test.Hspec
import Control.Monad
import Minimax.Minimax

inputs :: [(Int, Int)]
inputs = [(4, 4)]

tests :: [((Int, Int), String)]
tests = zip inputs $ map (uncurry bseq) inputs

testFun :: (Int -> Int -> String) -> Spec
testFun f = forM_ tests (\((i1, i2), o) -> it (show i1 ++ " " ++ show i2) $ f i1 i2 `shouldBe` o)

spec :: Spec
spec = do
    -- describe "seq" $ do
    describe "strategies" $ testFun bstrat
    -- describe "repa" $ testFun brepa
    describe "monad par" $ testFun bmpar
