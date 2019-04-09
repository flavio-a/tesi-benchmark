{-
Based on the idea of a gate array simulator but changed to make each gate a lot
more (computationally) expensive. Each gate is an Int^n -> Int function.
-}
-- {-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Gatesim.Gatesim
    ( bseq
    , bstrat
    , brepa
    , bmpar
    , Gate (..)
    , GateArray
    , toInput
    , fromList
    -- , compn
    -- , evalArray
    -- , getResult
    -- , sleep
    ) where

import Control.Monad
import qualified Data.Vector as Vector
import Data.Vector ((!), Vector)
-- import GHC.Generics (Generic)

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R
import Data.Functor.Identity

-- ================================== Common ==================================
{-
A gate has some inputs and one output. Inputs are denoted by integers, that are
indexes in the array of other gates whose output is connected to that input.
-}
data Gate = Input Int | Sum Int Int | Prod Int Int | Exp Int Int | Sleep Int
    -- deriving (Show,Generic,NFData)

-- Product "lazy" in its first argument, ie. doesn't evaluate the second if the
-- first is zero
(-*) :: (Num a, Eq a) => a -> a -> a
(!a) -* b = case a of
    0 -> 0
    _ -> a * b

-- Compose a function with itself n times. I worked a lot to make this
-- truly sequential because the runtime often ran it in parallel (on two cores)
compn :: Int -> (a -> a) -> a -> a
compn n f a | n <= 0    = a
            | otherwise = fa `seq` compn (n-1) f fa
                where
                    fa = f a

sleep :: Int -> Int
sleep !n = compn n (+1) 0

evalGate :: Vector Int -> Gate -> Int
evalGate res (Input n) = n
evalGate res (Sleep i) = sleep (res ! i)
evalGate res (Sum i j) = (res ! i) + (res ! j)
evalGate res (Prod i j) = (res ! i) -* (res ! j)
evalGate res (Exp i j) = (res ! i) ^ (res ! j)

{-
A gate array is an array of gates.
-}
type GateArray = Vector Gate

-------------------------------------------------------------------------------
-- Some utility functions to build gate arrays

-- Input builder: takes a list of 0/1 and maps it to a list of input gates
toInput :: [Int] -> [Gate]
toInput = map Input

fromList :: [Gate] -> GateArray
fromList = Vector.fromList

getResult :: Int -> [Gate] -> [Int]
getResult n = reverse . take n . reverse . Vector.toList . evalArray . Vector.fromList

-- ================================ Sequential ================================
bseq :: GateArray -> Int -> Int
bseq ga n = evalArray ga ! (length ga - n - 1)

-- Compute the value of each gate in the array.
-- Better in time for non-tree gate arrays: memoization of recursive calls
-- Worst for gate array with a useless part because its mapped over
-- (however it isn't evaluated baceause of lazyness)
evalArray :: GateArray -> Vector Int
evalArray ga = res
    where
        res = Vector.map (evalGate res) ga

-- ================================ Strategies ================================
bstrat :: GateArray -> Int -> Int
bstrat ga n = evalArrayStrat ga ! (length ga - n - 1)

-- Compute in parallel using strategies: each time a gate has two inputs spark
-- the computation of one of them.
-- This solution introduces great variance in execution time (depends on the
-- scheduling) and doesn't get good results on average
evalArrayStrat :: GateArray -> Vector Int
evalArrayStrat ga = res
    where
        res = Vector.map (evalGateStrat res) ga

evalGateStrat :: Vector Int -> Gate -> Int
evalGateStrat res g@(Sum i j) = res ! j `par` res ! i `pseq` evalGate res g
evalGateStrat res g@(Prod i j) = res ! j `par` res ! i `pseq` evalGate res g
evalGateStrat res g@(Exp i j) = res ! j `par` res ! i `pseq` evalGate res g
evalGateStrat res g = evalGate res g

-- ================================ Monad Par ================================
bmpar :: GateArray -> Int -> Int
bmpar ga n = runPar $ do
    ivars' <- replicateM (Vector.length ga) new
    let ivars = Vector.fromList ivars'
    mapM_ (fork . evalGateMpar ivars) $ Vector.zip ivars ga
    get (ivars ! (Vector.length ga - n - 1))

evalGateMpar :: Vector (IVar Int) -> (IVar Int, Gate) -> Par ()
evalGateMpar res (iv, Input a) = put iv a
evalGateMpar res (iv, Sum i j) = liftM2 (+) (get (res ! i)) (get (res ! j)) >>= put iv
evalGateMpar res (iv, Prod i j) = liftM2 (-*) (get (res ! i)) (get (res ! j)) >>= put iv
evalGateMpar res (iv, Sleep i) = get (res ! i) >>= (put iv . sleep)
evalGateMpar res (iv, Exp i j) = liftM2 (^) (get (res ! i)) (get (res ! j)) >>= put iv

-- =================================== Repa ===================================
brepa = bseq
