{-# LANGUAGE FlexibleInstances #-}
module Transclos.Transclos where

import Control.Monad
import Data.List
import Control.DeepSeq
import Debug.Trace

import Control.Parallel
import Control.Parallel.Strategies

-- ================================== Common ==================================
-- Compose a function with itself n times. I worked a lot to make this
-- truly sequential because the runtime often ran it in parallel (on two cores)
compn :: Int -> (a -> a) -> a -> a
compn n f a | n <= 0    = a
            | otherwise = fa `seq` compn (n-1) f fa
                where
                    fa = f a

sleep :: Int -> Int
sleep !n = compn n (+1) 0

-- Given that for parallelism to be useful a relation should be expensive this
-- relation is "weighted" with some useless computation.
-- n R m iff m = n + 1 and n < b
r1 :: Int -> Int -> [Int]
r1 b n | n < b     = sleep n `seq` [n+1]
       | otherwise = sleep b `seq` []

-- Concat a list of lists, then removes duplicates via sorting.
unionSort :: (Ord a) => [[a]] -> [a]
unionSort = map head . group . sort . concat

-- ================================ Sequential ================================
transcl :: (Ord a) => (a -> [a]) -> [a] -> [a]
transcl r xs = xs'
    where
        xs' = xs ++ build 0 (length xs)
        -- m and n is the interval that is used to generate new elements
        build m n = if null ys' then [] else ys' ++ build n (n + length ys')
            where ys' = filter (`notElem` take (n-1) xs') $ unionSort [ r y | y <- take (n-m) (drop m xs') ]

bseq :: (Ord a) => (a -> [a]) -> a -> [a] -> Bool
bseq r e seed = e `elem` transcl r seed

-- ================================ Strategies ================================
transclNested :: (Eq a) => (a -> [a]) -> [a] -> [[a]]  {- [a] -}
transclNested r xs = zss
    where -- zss :: [[a]]
    zss = xs : build 1 zss
    -- build :: Int -> [[a1]] -> [[a1]]
    build _ [] = []
    build j (xs:xss) = zss' ++ build (j + length zss') xss
        where
        zss' = [ filter (`notElem` concat (take j zss)) xs' | x <- xs, let xs' = r x ]

bstrat :: (Ord a, NFData a) => (a -> [a]) -> a -> [a] -> Bool
bstrat r e seed = e `elem` concat zss
    where
        zss = transclNested r seed `using` parBuffer 5 (evalList rdeepseq)

-- ================================ Monad Par ================================

-- =================================== Repa ===================================
