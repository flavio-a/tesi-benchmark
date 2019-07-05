{-
Based on the idea of a gate array simulator but changed to make each gate a lot
more (computationally) expensive. Each gate is an Int^n -> Int function.

Not based on any of the nofib benchmarks.
-}
-- {-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Gatesim.Gatesim
    ( bseq
    , bstrat
    -- , brepa
    , bmpar
    , Gate (..)
    , GateArray
    , toInput
    , fromList
    -- , vectorToRepa
    ) where

import Control.Monad
import qualified Data.Vector as Vector
import Data.Vector ((!), Vector)
-- import GHC.Generics (Generic)

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
-- import Data.Array.Repa.Index
-- import qualified Data.Array.Repa as R
-- import Data.Functor.Identity

-- ================================== Common ==================================
{-
A gate has some inputs and one output. Inputs are denoted by integers, that are
indexes in the array of other gates whose output is connected to that input.
-}
type Idx = Int
data Gate = Input Int | Sum Idx Idx | Prod Idx Idx | Exp Idx Idx | Sleep Idx
          | Min Idx Idx
    -- deriving (Show,Generic,NFData)

-- Product "lazy" in its first argument, ie. doesn't evaluate the second if the
-- first is zero
(-*) :: (Num a, Eq a) => a -> a -> a
(!a) -* b = case a of
    0 -> 0
    _ -> a * b

-- Sleep is the identity, but requires a computation proportional to the input
sleep :: Int -> Int
sleep !n = compn n (+1) 0
    where
        -- Compose a function with itself n times.
        compn :: Int -> (a -> a) -> a -> a
        compn n f a | n <= 0    = a
                    | otherwise = fa `seq` compn (n-1) f fa
                        where
                            fa = f a

-- Evaluates a gate in some vector of results (from which it takes inputs)
evalGate :: Vector Int -> Gate -> Int
evalGate res (Input n) = n
evalGate res (Sleep i) = sleep (res ! i)
evalGate res (Sum i j) = (res ! i) + (res ! j)
evalGate res (Prod i j) = (res ! i) -* (res ! j)
evalGate res (Exp i j) = (res ! i) ^ (res ! j)
evalGate res (Min i j) = min (res ! i) (res ! j)

{-
A gate array is an array of gates.
-}
type GateArray = Vector Gate

-------------------------------------------------------------------------------
-- Some utility functions

-- Input builder: takes a list of integers and maps it to a list of input gates
toInput :: [Int] -> [Gate]
toInput = map Input

-- Useful to avoid importing Data.Vector to use this function
fromList :: [Gate] -> GateArray
fromList = Vector.fromList

-- Used in debugging
getResult :: Int -> [Gate] -> [Int]
getResult n = reverse . take n . reverse . Vector.toList . evalArray . Vector.fromList

-- ================================ Sequential ================================
bseq :: GateArray -> Idx -> Int
bseq ga n = evalArray ga ! (length ga - n - 1)

-- Compute the value of each gate in the array. Useful becaus introduces
-- memoization, and unused gates aren't really computed because of laziness.
evalArray :: GateArray -> Vector Int
evalArray ga = res
    where
        res = Vector.map (evalGate res) ga

-- ================================ Strategies ================================
bstrat :: GateArray -> Idx -> Int
bstrat ga n = evalArrayStrat ga ! (length ga - n - 1)

-- Compute in parallel using strategies: each time a gate has two inputs spark
-- the computation of one of them.
-- This solution introduces great variance in execution time (depends on the
-- scheduling) and doesn't get much speedup on average.
evalArrayStrat :: GateArray -> Vector Int
evalArrayStrat ga = res
    where
        res = Vector.map (evalGateStrat res) ga

evalGateStrat :: Vector Int -> Gate -> Int
-- evalGateStrat res g@(Sum i j) = res ! j `par` res ! i `pseq` evalGate res g
-- evalGateStrat res g@(Prod i j) = res ! j `par` res ! i `pseq` evalGate res g
-- evalGateStrat res g@(Exp i j) = res ! j `par` res ! i `pseq` evalGate res g
evalGateStrat res (Sum i j) = iv + jv `using` strat
        where
            iv = res ! i
            jv = res ! j
            strat v = do rpar jv; rseq iv; return v
evalGateStrat res (Prod i j) = iv * jv `using` strat
        where
            iv = res ! i
            jv = res ! j
            strat v = do rpar jv; rseq iv; return v
evalGateStrat res (Exp i j) = iv ^ jv `using` strat
        where
            iv = res ! i
            jv = res ! j
            strat v = do rpar jv; rseq iv; return v
evalGateStrat res (Min i j) = min iv jv `using` strat
        where
            iv = res ! i
            jv = res ! j
            strat v = do rpar jv; rseq iv; return v
evalGateStrat res g = evalGate res g

-- Second version: parallel map on the res array with rdeepseq. Isn't much
-- better than sequential version (and depends on schedule)
evalArrayStrat2 :: GateArray -> Vector Int
evalArrayStrat2 ga = evalArray ga `using` parTraversable rdeepseq

-- Third version: parallel map on the res array, but only reduces to whnf.
-- Shouldn't get much more parallelism than strat2 because the computationally
-- expensives stages are only sleeps, that are strict in their argument.
evalArrayStrat3 :: GateArray -> Vector Int
evalArrayStrat3 ga = evalArray ga `using` parTraversable rseq

-- ================================ Monad Par ================================
-- Much better parallelism but always evaluates the whole array, even though
-- some gates may be unneeded.
bmpar :: GateArray -> Idx -> Int
bmpar ga n = runPar $ do
    ivars' <- replicateM (Vector.length ga) new
    let ivars = Vector.fromList ivars'
    mapM_ (fork . evalGateMpar ivars) $ Vector.zip ivars ga
    get (ivars ! (Vector.length ga - n - 1))

evalGateMpar :: Vector (IVar Int) -> (IVar Int, Gate) -> Par ()
evalGateMpar res (iv, Input a) = put iv a
evalGateMpar res (iv, Sleep i) = get (res ! i) >>= (put iv . sleep)
evalGateMpar res (iv, Sum i j) = liftM2 (+) (get (res ! i)) (get (res ! j)) >>= put iv
evalGateMpar res (iv, Prod i j) = liftM2 (-*) (get (res ! i)) (get (res ! j)) >>= put iv
evalGateMpar res (iv, Exp i j) = liftM2 (^) (get (res ! i)) (get (res ! j)) >>= put iv
evalGateMpar res (iv, Min i j) = liftM2 min (get (res ! i)) (get (res ! j)) >>= put iv

-- =================================== Repa ===================================
{-
I wans't able to write a parallel version of this using Repa. Maybe it's
possible, but I strongly doubt it because Repa wasn't thought to do such an
irregular parallelism.
-}
-- type GateUnbox = (Int, Int, Int)
--
-- gateUnbox :: Gate -> GateUnbox
-- gateUnbox (Input a) = (0, a, 0)
-- gateUnbox (Sleep i) = (1, i, 0)
-- gateUnbox (Sum i j) = (2, i, j)
-- gateUnbox (Prod i j) = (3, i, j)
-- gateUnbox (Exp i j) = (4, i, j)
--
-- gateBox :: GateUnbox -> Gate
-- gateBox (0, a, _) = Input a
-- gateBox (1, i, _) = Sleep i
-- gateBox (2, i, j) = Sum i j
-- gateBox (3, i, j) = Prod i j
-- gateBox (4, i, j) = Exp i j
--
-- -- Parallel map, more or less the same as strategies #2
-- brepa :: R.Array R.U DIM1 GateUnbox -> Idx -> Int
-- brepa ga n = evalArrayRepa ga R.! (Z :. lenga - n - 1)
--     where
--         lenga = case R.extent ga of
--             (Z :. i) -> i
--
-- evalArrayRepa :: R.Array R.U DIM1 GateUnbox -> R.Array R.U DIM1 Int
-- evalArrayRepa ga = res
--     where
--         res :: R.Array R.U DIM1 Int
--         res = R.computeS $ R.map (evalGateRepa res . gateBox) ga
--
-- evalGateRepa :: R.Source r Int => R.Array r DIM1 Int -> Gate -> Int
-- evalGateRepa res (Input a) = a
-- evalGateRepa res (Sleep i) = sleep (res R.! (Z :. i))
-- evalGateRepa res (Sum i j) = (res R.! (Z :. i)) + (res R.! (Z :. j))
-- evalGateRepa res (Prod i j) = (res R.! (Z :. i)) -* (res R.! (Z :. j))
-- evalGateRepa res (Exp i j) = (res R.! (Z :. i)) ^ (res R.! (Z :. j))
--
-- vectorToRepa :: GateArray -> R.Array R.U DIM1 GateUnbox
-- vectorToRepa ga = R.fromUnboxed (Z :. length ga) $ Vector.convert $ Vector.map gateUnbox ga
