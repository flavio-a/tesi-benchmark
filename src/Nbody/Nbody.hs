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
    ) where

import Control.Monad
import GHC.Conc (numCapabilities)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector, (!))
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

pairWiseAccel :: Float3D -> Float3D -> Float3D
{-# INLINE pairWiseAccel #-}
pairWiseAccel (x,y,z) (x',y',z') = multTriple factor (dx,dy,dz)
    where
        dx = x' - x
        dy = y' - y
        dz = z' - z
        distanceSq = dx^2 + dy^2 + dz^2 + eps
        factor = 1/sqrt(distanceSq ^ 3)

-- Generates one body in the system
genVector :: Int -> Float3D
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- Generates all bodies in the system
genInitVecs :: Int -> Vector Float3D
genInitVecs n = Vector.generate n genVector

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.
compute :: Vector Float3D -> Float3D -> Float3D
compute vecList vec = multTriple g (sx, sy, sz)
    where
        myvector = vec
        g = 9.8

        -- Making this much less haskell like to avoid allocation:
        end = Vector.length vecList

        (# sx, sy, sz #) = loop 0 0 0 0
        loop !i !ax !ay !az | i == end = (# ax, ay, az #)
                            | otherwise =
            let (px, py, pz) = pairWiseAccel myvector (vecList ! i)
            in loop (i+1) (ax+px) (ay+py) (az+pz)
-- compute vecList tag = multTriple g $ sumTriples $ Vector.map (pairWiseAccel myvector) vecList
--     where
--         myvector = vecList Vector.! tag
--         sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)

computeIndexed :: Vector Float3D -> Int -> Float3D
computeIndexed initVecs i = compute initVecs $ initVecs ! i

-- ================================ Sequential ================================
bseq :: Int -> [Float3D]
bseq n = map (computeIndexed initVecs) [0..n-1]
    where
        !initVecs = genInitVecs n

-- ================================ Strategies ================================
-- Parallel map chunking computations
bstrat :: Int -> [Float3D]
bstrat n = map (computeIndexed initVecs) [0..n-1] `using` parListChunk chunk rdeepseq
    where
        !initVecs = genInitVecs n
        chunk = numCapabilities * 10

-- ================================ Monad Par ================================
-- Parallel map chunking computations
bmpar :: Int -> [Float3D]
bmpar n = runPar $ do
    -- Control.Monad.Par.parMap (computeIndexed initVecs) [0..n-1]
    fs <- forM [0, chunk .. n-1] (spawnP . compChunk)
    ls <- mapM get fs
    return (concat ls)
    where
        !initVecs = genInitVecs n
        chunk = max 1 $ n `quot` (numCapabilities * 5)
        compChunk t = map (computeIndexed initVecs) [t .. min (t + chunk - 1) (n - 1)]

-- =================================== Repa ===================================
-- Parallel Repa operations, with no explicit clustering
brepa :: Int -> R.Array R.U DIM1 Float3D
brepa n = runIdentity $ R.computeP $ R.map (compute $ R.toUnboxed initVecs) initVecs
    where
        !initVecs = R.fromUnboxed (Z :. n) $ genInitVecs n

brepatest :: Int -> [Float3D]
brepatest = R.toList . brepa
