module Minimax.Minimax
    ( bseq
    -- , brepa
    , bstrat
    , bmpar
    ) where

import System.Random

import Minimax.Prog
import Minimax.Game
import Minimax.Board

bseq :: Int -> Int -> String
bseq n depth =
    let b = randomBoard (mkStdGen 99999) n in
        solve alternateSeq depth b

bstrat :: Int -> Int -> String
bstrat n depth =
    let b = randomBoard (mkStdGen 99999) n in
        solve alternateStrat depth b

bmpar :: Int -> Int -> String
bmpar n depth =
    let b = randomBoard (mkStdGen 99999) n in
        solve alternateMpar depth b
