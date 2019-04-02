-- {-# LANGUAGE ImplicitParams #-}
module NbodySpec (spec) where

import Test.Hspec
import Test.HUnit.Approx
import Control.Monad
import Nbody.Nbody

eps :: Float
eps = 0.05

inputs :: [Int]
inputs = [10, 20, 30]

tests :: [(Int, [Float3D])]
tests = zip inputs $ map bseq inputs

-- checkFloat3D :: Float3D -> Float3D -> Assertion
checkFloat3D (x1, x2, x3) (e1, e2, e3) = zipWithM_ (assertApproxEqual "" eps) [x1, x2, x3] [e1, e2, e3]

-- checkFloat3D :: [Float3D] -> [Float3D] -> Assertion
checkFloat3DList = zipWithM_ checkFloat3D

testFun :: (Int -> [Float3D]) -> Spec
testFun f = forM_ tests (\(i, o) -> it (show i) $ checkFloat3DList (f i) o)

spec :: Spec
spec = do
    -- describe "seq" $ do
    --     it "m1 * id" $ bseq m1 m2 `shouldBe` m1
    describe "strategies" $ testFun bstrat
    describe "repa" $ testFun brepatest
    -- describe "monad par" $ testFun bmpar
