module MatmultSpec (spec) where

import Test.Hspec
import Control.Monad
import Matmult.Matmult

showMat m = "<<" ++ unlines (map (concatMap (\x -> show x ++ " ")) m) ++ ">>"

m1, m2, m3 :: Matrix
m1 = splitGroup 4 [1..16]
m2 = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
m3 = replicate 4 [1..4]

m13 =
    [ [10, 20, 30, 40]
    , [26, 52, 78, 104]
    , [42, 84, 126, 168]
    , [58, 116, 174, 232]]

inputs :: [(Matrix, Matrix)]
inputs = [(a, b) | a <- [m1, m2, m3], b <- [m1, m3]]

tests :: [((Matrix, Matrix), Matrix)]
tests = zip inputs $ map (uncurry bseq) inputs

getNum :: Matrix -> String
getNum m | m == m1 = "m1"
         | m == m2 = "m2"
         | m == m3 = "m3"
         | otherwise = "m?"

showPair :: (Matrix, Matrix) -> String
showPair (a, b) = concat [getNum a, " * ", getNum b]

testFun :: ((Matrix, Matrix) -> Matrix) -> Spec
testFun f = forM_ tests (\(i, o) -> it (showPair i) $ f i `shouldBe` o)

spec :: Spec
spec = do
    describe "seq" $ do
        it "m1 * id" $ bseq m1 m2 `shouldBe` m1
        it "id * m1" $ bseq m2 m1 `shouldBe` m1
        it "m3 * id" $ bseq m3 m2 `shouldBe` m3
        it "m1 * m3" $ bseq m1 m3 `shouldBe` m13
    describe "strategies" $ testFun $ uncurry bstrat
    describe "repa" $ testFun $ uncurry brepa
    describe "monad par" $ testFun $ uncurry bmpar
