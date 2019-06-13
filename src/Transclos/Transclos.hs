{-# LANGUAGE FlexibleInstances, BangPatterns, ScopedTypeVariables #-}
module Transclos.Transclos where

import Control.Monad
import Data.List
import Control.DeepSeq
import Debug.Trace

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import Data.Array.Repa.Index
import Data.Functor.Identity
import Data.Vector.Unboxed.Base (Unbox)

-- ================================== Common ==================================
-- Compose a function with itself n times. Just to lose time
compn :: Int -> (a -> a) -> a -> a
compn n f a | n <= 0    = a
            | otherwise = fa `seq` compn (n-1) f fa
                where
                    fa = f a

sleep :: Int -> Int
sleep !n = compn n (+1) 0

-- Given that for parallelism to be useful a relation should be expensive these
-- relations are "weighted" with some useless computation.

-- n R m iff m = n + 1 and n < b
r1 :: Int -> Int -> [Int]
r1 b n | n < b     = sleep n `seq` [n+1]
       | otherwise = sleep b `seq` []

r2 :: Int -> [Int]
r2 n = sleep n `pseq` [n+1..n+11]

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
            where ys' = filter (`notElem` take (n-1) xs') $
                        unionSort [ r y | y <- take (n-m) (drop m xs') ]

bseq :: (Ord a) => (a -> [a]) -> a -> [a] -> Bool
bseq r e seed = e `elem` transcl r seed

-- ================================ Strategies ================================
transclNested :: (Eq a) => (a -> [a]) -> [a] -> [[a]]
transclNested r xs = zss
    where
    zss = xs : build 1 zss -- :: [[a]]
    -- build :: Int -> [[a]] -> [[a]]
    build _ [] = []
    build j (xs:xss) = zss' ++ build (j + length zss') xss
        where
        zss' = [ filter (`notElem` concat (take j zss)) xs' | x <- xs, let xs' = r x ]

bstrat :: (Ord a, NFData a) => (a -> [a]) -> a -> [a] -> Bool
bstrat r e seed = e `elem` concat zss
    where
        zss = transclNested r seed `using` parBuffer 8 rdeepseq
        -- zss = runEval $ do
        --         let (first, rest) = splitAt (length seeds) transclNested r seed
        --         rest' <- parBuffer n rdeepseq rest
        --         return (first ++ rest')

-- ================================ Monad Par ================================
{-
Monad par can't create an infinite data structur and then evaluate it lazily.
The solution is quite different. It has a list tc of elements already computed
and a list s of elements on which it hasn't applied r yet. It takes a bunch of
elements from the head of s and in parallel applies r on them, then it appends
these new elements to both tc and s.
-}
bmpar :: (Ord a, NFData a) => (a -> [a]) -> a -> [a] -> Bool
bmpar r e seed = runPar $ transMpar seed seed
    where
        transMpar tc s = do
            let (comp, rest) = splitAt 10 s
            l <- Control.Monad.Par.parMap r comp
            let l' = filter (`notElem` tc) $ concat l
                rest' = rest ++ l'
            if null rest' then return False else
                if e `elem` l' then return True else transMpar (tc ++ l') rest'


-- =================================== Repa ===================================
{-
More or less a copy of mpar.
-}
brepa :: forall a . (Ord a, NFData a, Unbox a) => (a -> [a]) -> a -> [a] -> Bool
brepa r e seed = transRepa seed seed
    where
        transRepa :: [a] -> [a] -> Bool
        transRepa tc s | null rest'  = False
                       | e `elem` l' = True
                       | otherwise   = transRepa (tc ++ l') rest'
            where
                (comp, rest) = splitAt 10 s
                l :: R.Array R.V DIM1 [a]
                l = runIdentity $ R.computeP $ R.map r $ R.fromListUnboxed (Z :. length comp) comp
                l' = filter (`notElem` tc) $ concat $ R.toList l
                rest' = rest ++ l'
