module MatmultVSpec (spec) where

import Test.Hspec
import Control.Monad
import Matmult.MatmultV
-- import qualified MatmultSpec as MS

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

m4, m5 :: Matrix
m4 = splitGroup 50 [1..2500]
m5 = idm 50

m1v, m2v, m3v, m4v, m5v :: MatrixV
[m1v, m2v, m3v, m4v, m5v] = map matrixToV [m1, m2, m3, m4, m5]

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

inputs :: [(MatrixV, MatrixV)]
inputs = map (mapPair matrixToV) $ [(a, b) | a <- [m1, m2, m3], b <- [m1, m3]] ++ [(m5, m4)]

tests :: [((MatrixV, MatrixV), Matrix)]
tests = zip inputs $ map (uncurry bseq) inputs

getNum :: MatrixV -> String
getNum m | m == m1v = "m1"
         | m == m2v = "id"
         | m == m3v = "m3"
         | m == m4v = "m4"
         | m == m5v = "id"
         | otherwise = "m?"

showPair :: (MatrixV, MatrixV) -> String
showPair (a, b) = concat [getNum a, " * ", getNum b]

testFun :: ((MatrixV, MatrixV) -> Matrix) -> Spec
testFun f = forM_ tests (\(i, o) -> it (showPair i) $ f i `shouldBe` o)

spec :: Spec
spec = do
    describe "seq" $ do
        it "m1 * id" $ bseq m1v m2v `shouldBe` m1
        it "id * m1" $ bseq m2v m1v `shouldBe` m1
        it "m3 * id" $ bseq m3v m2v `shouldBe` m3
        it "m1 * m3" $ bseq m1v m3v `shouldBe` m13
        it "id * m4" $ bseq m5v m4v `shouldBe` m4
    describe "strategies" $ testFun $ uncurry bstrat
    describe "repa" $ testFun $ uncurry brepatest
    describe "monad par" $ testFun $ uncurry bmpar
