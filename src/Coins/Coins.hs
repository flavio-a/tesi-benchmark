module Coins.Coins
    ( bseq
    , bstrat
    , bmpar
    , brepa
    ) where

import Data.List
import Control.Applicative
import Control.DeepSeq
import Data.Functor.Identity

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R

-- ================================ Sequential ================================
bseq :: Int -> [(Int, Int)] -> Int
bseq = payN

-- Simple algorithm: for each coin value recursively calls itself either picking
-- a coin with that value (left branch) or not (right)
payN :: Int -> [(Int, Int)] -> Int
payN 0 _  = 1
payN _ [] = 0
payN val ((c,q):coins)
    | c > val   = payN val coins
    | otherwise = left + right
    where
        left  = payN (val - c) coins'
        right = payN val coins
        coins' | q == 1    = coins
               | otherwise = (c,q-1) : coins

-- ================================ Strategies ================================
bstrat :: Int -> [(Int, Int)] -> Int
bstrat = payNstrat th
    where
        th = 7 -- threshold parameter

-- Sparks evalutaion of left branches until it reaches the threshold, then
-- concludes the exploration sequentially
payNstrat :: Int -> Int -> [(Int, Int)] -> Int
payNstrat 0 val coins = payN val coins
payNstrat _ 0 _  = 1
payNstrat _ _ [] = 0
payNstrat th val ((c,q):coins)
    | c > val   = payNstrat th val coins
    | otherwise = left `par` right `pseq` right + left
    where
        left  = payNstrat (th - 1) (val - c) coins'
        right = payNstrat (th - 1) val coins
        coins' | q == 1    = coins
               | otherwise = (c,q-1) : coins

-- ================================ Monad Par ================================
bmpar :: Int -> [(Int, Int)] -> Int
bmpar c coins = runPar $ payNmpar th c coins
    where
        th = 10 -- threshold parameter

-- Same parallelism as for strategies
payNmpar :: Int -> Int -> [(Int, Int)] -> Par Int
payNmpar 0 val coins = return $ payN val coins
payNmpar _ 0 coins = return 1
payNmpar _ _ [] = return 0
payNmpar th val ((c,q):coins)
    | c > val   = payNmpar th val coins
    | otherwise = do
        leftI <- spawn $ payNmpar (th - 1) (val - c) coins'
        right <- payNmpar (th - 1) val coins
        left <- get leftI
        return $ left + right
    where
        coins' | q == 1    = coins
               | otherwise = (c, q-1) : coins

-- =================================== Repa ===================================
-- Sequential + parallelized sequential tails
brepa :: Int -> [(Int, Int)] -> Int
brepa val coins = runIdentity result
    where
        states = payNrepa th coins (val, 0, 0) -- List of tail starting points
        statesR = R.fromListUnboxed (Z :. length states) states
        result = R.sumAllP $ R.map (payNstate coins) statesR
        th = 10 -- threshold

-- Tuples can be unboxed, so it's possible to create a Repa array of tuples
-- (val, elements dropped in coins, coins of the first type left used)
type CoinState = (Int, Int, Int)

-- Given the original set of coins and a CoinState computes the actual set of
-- coins
actualcoins :: [(Int, Int)] -> CoinState -> [(Int, Int)]
actualcoins coins (_, idx, q) = rephead q (drop idx coins)
    where
        rephead :: Int -> [(a, Int)] -> [(a, Int)]
        rephead _ [] = []
        rephead n ((a, q):xs) = (a, q - n):xs

-- Sequential function for evaluation of a tail: from a CoinState returns the
-- number of ways to pay the remaining value with remaining coins
payNstate :: [(Int, Int)] -> CoinState -> Int
payNstate coins cs@(val, _, _) = payN val $ actualcoins coins cs

-- Computes the set of states after th steps, that are tails' starting points,
-- then maps in parallel
payNrepa :: Int -> [(Int, Int)] -> CoinState -> [CoinState]
payNrepa 0 _ cs = [cs]
payNrepa _ _ cs@(0, _, _) = [cs]
payNrepa th coins cs@(val, idx, q)
    | null coins' = []
    | c1 > val    = payNrepa th coins (val, idx + 1, 0)
    | otherwise   = left ++ right
    where
        coins' = actualcoins coins cs
        (c1, q1) = head coins'
        left  = payNrepa (th - 1) coins cs'
        right = payNrepa (th - 1) coins (val, idx + 1, 0)
        cs' | q1 == 1   = (val - c1, idx + 1, 0)
            | otherwise = (val - c1, idx, q + 1)
