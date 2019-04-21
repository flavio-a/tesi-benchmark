module MatmultSpec (spec) where

import Test.Hspec
import Control.Monad
import Matmult.Matmult

showMat :: Matrix -> String
showMat = unlines . map (unwords . map show)

-- Create identity matrix of rank n
idm :: Int -> Matrix
idm n = [ [fromEnum $ i == j | i <- [1..n] ] | j <- [1..n] ]

m1, m2, m3, m13 :: Matrix
m1 = splitGroup 4 [1..16]
m2 = idm 4
m3 = replicate 4 [1..4]

m13 =
    [ [10, 20, 30, 40]
    , [26, 52, 78, 104]
    , [42, 84, 126, 168]
    , [58, 116, 174, 232]]

m4 :: Matrix
m4 = splitGroup 50 [1..2500]
m5 = idm 50

inputs :: [(Matrix, Matrix)]
inputs = [(a, b) | a <- [m1, m2, m3], b <- [m1, m3]] ++ [(m5, m4)]

tests :: [((Matrix, Matrix), Matrix)]
tests = zip inputs $ map (uncurry bseq) inputs

getNum :: Matrix -> String
getNum m | m == m1 = "m1"
         | m == m2 = "id"
         | m == m3 = "m3"
         | m == m4 = "m4"
         | m == m5 = "id"
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
        it "id * m4" $ bseq m5 m4 `shouldBe` m4
    describe "strategies" $ testFun $ uncurry bstrat
    describe "repa" $ testFun $ uncurry brepatest
    describe "monad par" $ testFun $ uncurry bmpar
