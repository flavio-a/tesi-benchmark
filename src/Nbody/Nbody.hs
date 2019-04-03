{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc.,
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 - Original author: Chih-Ping Chen
 -}
{-# LANGUAGE CPP, UnboxedTuples, BangPatterns, FlexibleContexts #-}
module Nbody.Nbody
    ( bseq
    , bstrat
    , bmpar
    , brepa
    , brepatest
    , Float3D
    , genInitVecs
    , genInitVecsRepa
    ) where

import Control.Monad
import GHC.Conc (numCapabilities)
import qualified Data.Array as Array
import Data.Functor.Identity

import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R

-- ================================== Common ==================================
eps = 0.005 :: Float
g = 9.8 :: Float

type Float3D = (Float, Float, Float)

multTriple :: Float -> Float3D -> Float3D
{-# INLINE multTriple #-}
multTriple c ( x,y,z ) = ( c*x,c*y,c*z )

sumTriples :: Float3D -> Float3D -> Float3D
{-# INLINE sumTriples #-}
sumTriples (x,y,z) (x',y',z') = (x+x',y+y',z+z')

-- Generates one body in the system
genVector :: Int -> Float3D
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- Generates all bodies in the system
genInitVecs :: Int -> Array.Array Int Float3D
genInitVecs n = Array.array (0, n-1) [ (i, genVector i) | i <- [0..n-1] ]

pairWiseAccel :: Float3D -> Float3D -> Float3D
{-# INLINE pairWiseAccel #-}
pairWiseAccel (x,y,z) (x',y',z') = multTriple factor (dx,dy,dz)
    where
        dx = x' - x
        dy = y' - y
        dz = z' - z
        distanceSq = dx^2 + dy^2 + dz^2 + eps
        factor = 1/sqrt(distanceSq ^ 3)

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.
compute :: Array.Array Int Float3D -> Int -> Float3D
-- compute vecList tag = accel myvector vecList
--     where
--         myvector = vecList Array.! tag
--         g = 9.8
--
--         -- Making this much less haskell like to avoid allocation:
--         (strt,end) = Array.bounds vecList
--
--         accel :: Float3D -> Array.Array Int Float3D -> Float3D
--         accel vector vecList = multTriple g (sx, sy, sz)
--             where
--                 (# sx, sy, sz #) = loop strt 0 0 0
--                 loop !i !ax !ay !az | i == end = (# ax, ay, az #)
--                                     | otherwise =
--                     let (px, py, pz) = pairWiseAccel vector (vecList Array.! i)
--                     in loop (i+1) (ax+px) (ay+py) (az+pz)
compute vecList tag = accel myvector vecList
    where
        myvector = vecList Array.! tag

        sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)
        accel vector vecList = multTriple g $ sumTriples $ map (pairWiseAccel vector) $ Array.elems vecList

-- ================================ Sequential ================================
bseq :: Int -> [Float3D]
bseq n = map (compute initVecs) [0..n-1]
    where
        initVecs = genInitVecs n

-- ================================ Strategies ================================
-- Parallel map chunking computations
bstrat :: Int -> [Float3D]
bstrat n = map (compute initVecs) [0..n-1] `using` parListChunk chunk rdeepseq
    where
        initVecs = genInitVecs n
        -- 10 chunks per Capability
        chunk = numCapabilities * 10

-- ================================ Monad Par ================================
-- Parallel map chunking computations
bmpar :: Int -> [Float3D]
bmpar n = runPar $ do
    let initVecs = genInitVecs n
        chunk = n `quot` (numCapabilities * 5)
    -- Control.Monad.Par.parMap (compute initVecs) [0..n-1]
    fs <- forM [0, chunk .. n-1] $ \t -> do
        let t1 = min (t + chunk - 1) (n - 1)
        spawnP $ map (compute initVecs) [t .. t1]
    ls <- mapM get fs
    return (concat ls)

-- =================================== Repa ===================================
-- Parallel Repa operations, with no explicit clustering

-- Generates all bodies in the system in a Repa array
genInitVecsRepa :: Int -> R.Array R.U DIM1 Float3D
genInitVecsRepa n = R.computeS $ R.map genVector $ R.fromListUnboxed (Z :. n) [0..n-1]

brepa :: Int -> R.Array R.U DIM1 Float3D
brepa n = runIdentity $ R.computeP $ R.map computeRepa initVecs
    where
        initVecs = genInitVecsRepa n

        -- function "compute" modified to work on Repa arrays
        computeRepa :: Float3D -> Float3D
        {-# INLINE computeRepa #-}
        computeRepa vector = multTriple g (R.foldS sumTriples (0, 0, 0) accs R.! Z)
            where
                accs = R.map (pairWiseAccel vector) initVecs :: R.Array R.D DIM1 Float3D

brepatest :: Int -> [Float3D]
brepatest = R.toList . brepa
