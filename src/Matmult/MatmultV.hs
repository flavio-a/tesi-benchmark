{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Matmult.MatmultV
    ( bseq
    , brepa
    , bstrat
    , bmpar
    , brepatest
    , MatrixV
    , Matrix
    , splitGroup
    , splitGroupV
    , concatV
    , matrixToV
    ) where

import Data.List
import Control.DeepSeq
import Control.Applicative
import Control.Monad
import qualified Data.Vector.Unboxed as V

import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as R
import Data.Functor.Identity

-- ================================== Common ==================================
type Matrix = [[Int]]
type MatrixV = [V.Vector Int]

splitGroupV :: V.Unbox a => Int -> [a] -> [V.Vector a]
splitGroupV _ [] = []
splitGroupV n xs = V.fromList h : splitGroupV n t
    where (h, t) = splitAt n xs

concatV :: V.Unbox a => [V.Vector a] -> V.Vector a
concatV = V.concat

matrixToV :: Matrix -> MatrixV
matrixToV = map V.fromList

transposeV :: MatrixV -> MatrixV
transposeV [] = []
transposeV ls = matrixToV $ transposeV' ls
    where
        transposeV' vs | V.null $ head vs = []
                       | otherwise = map V.head vs : transposeV' (map V.tail vs)

-- Split a matrix in c x c blocks
splitIntoClusters :: Int -> Matrix -> [[Matrix]]
splitIntoClusters c m | c < 1 = splitIntoClusters 1 m
splitIntoClusters c m1 = mss
    where
        bh = kPartition (length m1) c
        bhsplit [] [] = []
        bhsplit [] _  = error "some elements left over"
        bhsplit (t:ts) xs = hs : bhsplit ts rest
            where (hs,rest) = splitAt t xs
        ms  = bhsplit bh m1 -- blocks of rows
        mss = map (colsplit bh) ms

        colsplit [] _  = []
        colsplit (t:ts) rs
            | null $ head rs = []
            | otherwise = cab : colsplit ts resto
            where  (cab,resto) = unzip (map (splitAt t) rs)

        kPartition :: Int -> Int -> [Int]
        kPartition n k = zipWith (+) (replicate (n `mod` k) 1 ++ repeat 0)
                                     (replicate k (n `div` k))

mergeBlocks :: [[Matrix]] -> Matrix
mergeBlocks = concatMap rowize
    where
        rowize :: [Matrix] -> [ [Int] ]
        rowize ms | null ms        = []
                  | null $ head ms = []
                  | otherwise      = concatMap head ms : rowize (map tail ms)

dotProd :: V.Vector Int -> V.Vector Int -> Int
dotProd !v1 = V.ifoldl' (\acc idx v -> acc + v * v1 V.! idx) 0

-- Assumes the second matrix has already been transposed
multMatricesTr :: MatrixV -> MatrixV -> Matrix
multMatricesTr !m1 !m2 = [ map (dotProd row) m2 | row <- m1]

-- ================================ Sequential ================================
bseq :: MatrixV -> MatrixV -> Matrix
bseq m1 m2 = multMatricesTr m1 (transposeV m2)

-- ================================ Strategies ================================
-- Clustering in blocks. Better performances than rows/colums clustering
bstrat :: MatrixV -> MatrixV -> Matrix
bstrat m1 m2 = multMatricesTr m1 (transposeV m2) `using` blockStrat 20
    where
        blockStrat :: Int -> Strategy Matrix
        blockStrat c matrix = mergeBlocks <$> parList (parList rdeepseq) blocks
            where
                blocks = splitIntoClusters numB matrix
                numB  = length matrix `div` c

-- ================================ Monad Par ================================
-- Different clustering, in rows
bmpar :: MatrixV -> MatrixV -> Matrix
bmpar m1 m2 = runPar $ Control.Monad.Par.parMap (\row -> map (dotProd row) m2') m1
    where
        m2' = transposeV m2

-- =================================== Repa ===================================
-- Different clustering, each element in parallel
splitGroup :: V.Unbox a => Int -> [a] -> [[a]]
splitGroup _ [] = []
splitGroup n xs = h : splitGroup n t
    where (h, t) = splitAt n xs

-- Wrapper of brepa for testing
brepatest :: MatrixV -> MatrixV -> Matrix
brepatest m1 m2 = arr2mat $ brepa m1' m2'
    where
        l = length m1
        shll = Z :. l :. l
        arr2mat :: R.Shape sh => R.Array R.U sh Int -> Matrix
        arr2mat = splitGroup l . R.toList
        m1' = R.fromUnboxed shll $ V.concat m1 :: R.Array R.U DIM2 Int
        m2' = R.fromUnboxed shll $ V.concat m2 :: R.Array R.U DIM2 Int

-- See also the library implementation: Data.Array.Repa.Algorithms.Matrix.mmultP
brepa :: R.Array R.U DIM2 Int -> R.Array R.U DIM2 Int -> R.Array R.U DIM2 Int
-- brepa m1 m2 = R.computeS $ R.fromFunction shll (prodrc m1 m2')
brepa m1 m2 = runIdentity $
    R.computeP $ m2' `R.deepSeqArray` R.fromFunction shll (prodrc m1 m2')
    where
        shll = R.extent m1
        m2' = R.computeS $ R.transpose m2 :: R.Array R.U DIM2 Int

        prodrc :: R.Array R.U DIM2 Int -> R.Array R.U DIM2 Int -> DIM2 -> Int
        {-# INLINE prodrc #-}
        prodrc m1 m2 (Z :. i :. j) = R.sumAllS $ R.zipWith (*) (getRow i m1) (getRow j m2)
        getRow :: R.Source r a => Int -> R.Array r DIM2 a -> R.Array R.D DIM1 a
        {-# INLINE getRow #-}
        getRow i v = R.unsafeSlice v (R.Any :. i :. R.All)
