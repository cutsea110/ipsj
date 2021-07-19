module Ex460504 where

import Data.List
import Ticket hiding (trees)

trees :: [Char] -> [Char] -> [Term]
trees ds os = [ t | (_, t) <- trees' ds os ]

trees' :: [Char] -> [Char] -> [([Char], Term)]
trees' [c] os = [(os, Val c)]
trees' ds  os = concat [ odtree os xs ys | (xs, ys) <- splits1 ds ]

odtree :: [Char] -> [Char] -> [Char] -> [([Char], Term)]
odtree os ls rs
  = [ (os'', App o l r)
    | (o:os', l) <- trees' ls os
    , (os'',  r) <- trees' rs os' ]
