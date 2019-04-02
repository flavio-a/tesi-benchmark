{-# LANGUAGE FlexibleInstances #-}
module Transclos.Transclos where

import Control.Monad
import Data.List
import Control.DeepSeq
import Debug.Trace

import Control.Parallel
import Control.Parallel.Strategies

-- ================================== Common ==================================
r1 :: Int -> Int -> [Int]
r1 b n | n<b       = [n+1]  -- n R m iff m = n + 1 and n < b
       | otherwise = []

-- r1_set b n = Data.Set.fromList (r1 b n)

-- n R m iff m is an even number less than n and n < b
r2 :: Int -> Int -> [Int]
r2 b n | n<b       = [ m | m <- [(n-1),(n-2)..1] , even m ]
       | otherwise = []

-- r2_set b n = Data.Set.fromList (r2 b n)

-- n R m iff m is 2*n, 3*n or 5*n. Creates the set of numbers that factorizes
-- with onyl 2, 3 and 5
r3 :: Int -> [Int]
r3 n = map (* n) [2, 3, 5]

-- r3_set = Data.Set.fromList . r3

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

bseq :: (Ord a) => (a -> [a]) -> [a] -> [a]
bseq = transcl

-- -- circular version, using sets rather than lists
-- transclSet :: (Ord a, Eq a) => (a -> Data.Set.Set a) -> Data.Set.Set a -> Data.Set.Set a
-- transclSet r xs = foldl Data.Set.union Data.Set.empty xs'
--     where
--         xs' = xs : build xs 1
--         -- build :: (Ord a, Eq a) => Data.Set.Set a -> Int -> [Data.Set.Set a]
--         build s n = if Data.Set.null ys'
--                         then []
--                         else ys' : build ys' (n+1)
--                     where
--                         ys' = Data.Set.filter (is_new ys0) $ foldl Data.Set.union Data.Set.empty [ ys | y <- Data.Set.toList s, let ys = r y ]
--                         ys0 = take n xs'
--
--                         is_new [] y                                = True
--                         is_new (xs:xss) y | y `Data.Set.member` xs = False
--                                           | otherwise              = is_new xss y
--
--
-- -- transclSet (r1_set 444) (Data.Set.fromList [1])
-- -- (0.07 secs, 3884380 bytes)
-- -- ca 3.8MB

-- ================================ Strategies ================================
-- main parallel version:
-- producing a list-of-list improves parallelism, since the position of an element
-- does not depend on all the previous elements
transclNested :: (Ord a) => (a -> [a]) -> [a] -> [[a]]
transclNested r xs = zss
    where
        -- zss :: [[a]]
        zss = xs : build 1 xs
        -- build :: Int -> [a] -> [[a]]
        build j ys = if null $ concat ys' then [] else ys' ++ concat others
            where
                -- length ys' == length ys
                -- ys' :: [[a]]
                ys' = map (filter (`notElem` concat (take j zss)) . r) ys
                lengths = scanl (+) 0 $ map length ys'
                others = map (\(idx, ws) -> build (j + length ys + (lengths !! idx)) ws) $ zip [0..] ys'

-- transclNested :: (Ord a) => (a -> [a]) -> [a] -> [[a]]
-- transclNested r xs = zss
--     where
--         -- zss :: [[a]]
--         zss = xs : build 1 xs
--         -- build :: Int -> [a] -> [[a]]
--         build j xs = if null ys' then [] else ys' : build (j + 1) ys'
--             where
--                 ys' = filter (`notElem` concat (take j zss)) $ unionSort $ map r xs
--                 -- ys' = unionSort $ map (filter (`notElem` concat (take j zss)) . r) xs

-- main parallel version:
-- producing a list-of-list improves parallelism, since the position of an element
-- does not depend on all the previous elements
transcl_nested :: (Eq a) => (a -> [a]) -> [a] -> [[a]]  {- [a] -}
transcl_nested r xs = zss
    where -- zss :: [[a]]
    zss = xs : build 1 zss
    -- build :: Int -> [[a]] -> [[a]]
    build j (xs:xss) = zss' ++ build (j + length zss') xss
        where
        zss' = [ filter (`notElem` concat (take j zss)) xs' | x <- xs, let xs' = r x ]

bstrat :: (Ord a, NFData a) => (a -> [a]) -> [a] -> [[a]]
bstrat r xs = transcl_nested r xs `using` parBuffer 10 (evalList rseq)

-- bstrat :: (Ord a, NFData a) => (a -> [a]) -> [a] -> [[a]]
-- bstrat r xs = zss `using` evalList (evalList rseq)
--     where
--         -- zss :: [[a]]
--         zss = xs : build 1 xs
--         -- build :: Int -> [a] -> [[a]]
--         build j xs = if null ys' then [] else ys' : build (j + 1) ys'
--             where
--                 ys' = filter (`notElem` concat (take j zss)) (unionSort $ map r xs) `using` parList rdeepseq

-- ================================ Monad Par ================================

-- =================================== Repa ===================================
