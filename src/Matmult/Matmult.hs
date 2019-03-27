{-# LANGUAGE BangPatterns #-}

module Matmult.Matmult where

import Data.List
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity

import Matmult.ListAux

import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R

-- ================================== Common ==================================
type Vector = [Int]
type Matrix = [Vector]

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

concatClusters :: [[Matrix]] -> Matrix
concatClusters mss = m
    where
        m = concatMap concatLine mss
        concatLine :: [Matrix] -> [Vector]
        concatLine ms | null (head ms) = []
                      | otherwise = concatMap head ms : concatLine (map tail ms)

addMatrices :: Matrix -> Matrix -> Matrix
addMatrices = zipWith addVectors
    where
        addVectors :: Vector -> Vector -> Vector
        addVectors = zipWith (+)

prodEscalar2 :: Vector -> Vector -> Int
prodEscalar2 v1 v2 = addProd v1 v2 0
    where
    addProd :: Vector -> Vector -> Int -> Int
    addProd (v:vs) (w:ws) !acc = addProd vs ws (acc + v * w)
    addProd _ _ !n = n

-- Assumes the second matrix has already been transposed
multMatricesTr :: Matrix -> Matrix -> Matrix
multMatricesTr m1 m2 = [[prodEscalar2 row col | col <- m2] | row <- m1]

prodEscalar2Block :: [Matrix] -> [Matrix] -> Matrix
prodEscalar2Block v1 v2 = addProd v1 v2 (repeat $ repeat 0)
    where
    addProd :: [Matrix] -> [Matrix] -> Matrix -> Matrix
    addProd (v:vs) (w:ws) acc = addProd vs ws (addMatrices acc (multMatricesTr v (transpose w)))
    addProd _ _ n = n

-- ================================ Sequential ================================
bseq :: Matrix -> Matrix -> Matrix
bseq m1 m2 = multMatricesTr m1 (transpose m2)

-- ================================ Strategies ================================
blockStrat :: Int -> Strategy Matrix
blockStrat c matrix
    = let blocks = concat (splitIntoClusters numB matrix) -- result splitted
                                                          -- in numB * numB blocks
          numB  = length matrix `div` c
      in fmap concat $ parList rdeepseq blocks

bstrat :: Matrix -> Matrix -> Matrix
bstrat m1 m2 = multMatricesTr m1 (transpose m2) `using` blockStrat 20

-- ================================ Monad Par ================================
bmpar :: Matrix -> Matrix -> Matrix
bmpar m1 m2 = runPar $ do
    r <- parMapM (\b1s -> Control.Monad.Par.parMap (prodEscalar2Block b1s) m2s) m1s
    return $ concatClusters r
    where
        numB = 10
        m1s = splitIntoClusters numB m1
        m2s = transpose $ splitIntoClusters numB m2
        -- blocks = [[prodEscalar2Block b1s b2s | b2s <- m2s] | b1s <- m1s]

-- =================================== Repa ===================================
-- Change clustering: compute each element in parallel

splitGroup :: Int -> [a] -> [[a]]
splitGroup _ [] = []
splitGroup n xs = h : splitGroup n t
    where (h, t) = splitAt n xs

brepa :: Matrix -> Matrix -> Matrix
brepa m1 m2 = arr2mat $ runIdentity $ R.computeP r
    where
        l = length m1
        shll = Z :. l :. l
        m1' = R.fromListUnboxed shll $ concat m1 :: R.Array R.U DIM2 Int
        m2' = R.transpose $ R.fromListUnboxed shll $ concat m2 :: R.Array R.D DIM2 Int
        r = R.fromFunction shll (\(Z :. i :. j) -> getRow i m1' `dotprod` getRow j m2') :: R.Array R.D DIM2 Int

        dotprod v1 v2 = R.sumS (R.zipWith (*) v1 v2) R.! Z
        getRow :: R.Source r a => Int -> R.Array r DIM2 a -> R.Array R.D DIM1 a
        getRow i v = R.slice v (R.Any :. i :. R.All)
        arr2mat :: R.Shape sh => R.Array R.U sh Int -> Matrix
        arr2mat = splitGroup l . R.toList
