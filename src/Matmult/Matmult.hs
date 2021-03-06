{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Matmult.Matmult
    ( bseq
    , brepa
    , bstrat
    , bmpar
    , brepatest
    , Matrix
    , splitGroup
    ) where

import Data.List
import Control.DeepSeq
import Control.Applicative
import Control.Monad

import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Unsafe as R
import Data.Functor.Identity

-- ================================== Common ==================================
type Vector = [Int]
type Matrix = [Vector]

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
        rowize :: [Matrix] -> [Vector]
        rowize ms | null ms        = []
                  | null $ head ms = []
                  | otherwise      = concatMap head ms : rowize (map tail ms)

prodEscalar2 :: Vector -> Vector -> Int
prodEscalar2 v1 v2 = addProd v1 v2 0
    where
    addProd :: Vector -> Vector -> Int -> Int
    addProd (v:vs) (w:ws) !acc = addProd vs ws (acc + v * w)
    addProd _ _ !n = n

-- Assumes the second matrix has already been transposed
multMatricesTr :: Matrix -> Matrix -> Matrix
multMatricesTr m1 m2 = [[prodEscalar2 row col | col <- m2] | row <- m1]

-- ================================ Sequential ================================
-- Using the strategy only to make computation strict (no parallelism
-- is introduced)
bseq :: Matrix -> Matrix -> Matrix
bseq m1 m2 = multMatricesTr m1 (transpose m2) `using` seqBlockStrat 20
    where
        seqBlockStrat :: Int -> Strategy Matrix
        seqBlockStrat c matrix = mergeBlocks <$> rdeepseq blocks
            where
                blocks = splitIntoClusters numB matrix
                numB  = length matrix `div` c

-- ================================ Strategies ================================
-- Clustering in blocks. Better performances than rows/colums clustering
blockStrat :: Int -> Strategy Matrix
blockStrat c matrix = mergeBlocks <$> parList (parList rdeepseq) blocks
    where
        blocks = splitIntoClusters numB matrix
        numB  = length matrix `div` c

bstrat :: Matrix -> Matrix -> Matrix
bstrat m1 m2 = multMatricesTr m1 (transpose m2) `using` blockStrat 20

-- ================================ Monad Par ================================
-- Different clustering, in rows. Again, better performances. Again, strategies
-- used only to introduce strictness (doesn't really change performances).
bmpar :: Matrix -> Matrix -> Matrix
bmpar m1 m2 = runPar $ Control.Monad.Par.parMap (\row -> map (prodEscalar2 row) m2' `using` rdeepseq) m1
    where
        m2' = transpose m2

-- =================================== Repa ===================================
-- Different clustering, each element in parallel. Repa makes a lot harder to
-- cluster at different levels.
splitGroup :: Int -> [a] -> [[a]]
splitGroup _ [] = []
splitGroup n xs = h : splitGroup n t
    where (h, t) = splitAt n xs

-- Wrapper of brepa for testing
brepatest :: Matrix -> Matrix -> Matrix
brepatest m1 m2 = arr2mat $ brepa m1' m2'
    where
        l = length m1
        shll = Z :. l :. l
        arr2mat :: R.Shape sh => R.Array R.U sh Int -> Matrix
        arr2mat = splitGroup l . R.toList
        m1' = R.fromListUnboxed shll $ concat m1 :: R.Array R.U DIM2 Int
        m2' = R.fromListUnboxed shll $ concat m2 :: R.Array R.U DIM2 Int

-- See also the library implementation: Data.Array.Repa.Algorithms.Matrix.mmultP
brepa :: R.Array R.U DIM2 Int -> R.Array R.U DIM2 Int -> R.Array R.U DIM2 Int
-- brepa m1 m2 = R.computeS $ R.fromFunction shll (prodrc m1 m2')
brepa m1 m2 = runIdentity $
    -- m2' <- R.computeP m2'' :: Identity (R.Array R.U DIM2 Int)
    R.computeP $ m2' `R.deepSeqArray` R.fromFunction shll (prodrc m1 m2')
    where
        shll = R.extent m1
        m2'' = R.transpose m2 :: R.Array R.D DIM2 Int
        m2' = R.computeS m2'' :: R.Array R.U DIM2 Int

        prodrc :: R.Array R.U DIM2 Int -> R.Array R.U DIM2 Int -> DIM2 -> Int
        {-# INLINE prodrc #-}
        prodrc m1 m2 (Z :. i :. j) = R.sumAllS $ R.zipWith (*) (getRow i m1) (getRow j m2)
        getRow :: R.Source r a => Int -> R.Array r DIM2 a -> R.Array R.D DIM1 a
        {-# INLINE getRow #-}
        getRow i v = R.unsafeSlice v (R.Any :. i :. R.All)
