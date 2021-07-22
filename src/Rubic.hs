module Rubic where

import Data.List

data Obj = T | S | E | B | N | W deriving (Eq, Enum, Show)
type Perm = [(Obj, Obj)]
type Cycle = [Obj]
type CyclePerm = [Cycle]

allObj :: [Obj]
allObj = [T .. W]

go :: Obj -> Cycle -> Obj
go o c = case elemIndices o c of
  []  -> o
  [i] -> cycle c !! succ i

goesTo :: Obj -> CyclePerm -> Obj
goesTo = foldl go

assoc :: Eq k => k -> [(k, v)] -> [(k, v)]
assoc c as = [(k, v) | (k, v) <- as, c == k]

makeCycle0 :: Perm -> CyclePerm -> CyclePerm
makeCycle0 [] qs = qs
makeCycle0 ((x, y):ss) qs
  | x == y    = makeCycle0 ss qs
  | otherwise = makeCycle0 ss ([x,y]:qs)

makeCycle1 :: Perm -> CyclePerm -> CyclePerm
makeCycle1 ss (cs:css)
  | c == head cs = makeCycle0 ss' (cs:css)
  | otherwise    = makeCycle1 ss' ((cs ++ [c]):css)
  where
    c   = snd d
    d   = head (assoc (last cs) ss)
    ss' = delete d ss

prodPerm :: [CyclePerm] -> CyclePerm
prodPerm ops = makeCycle0 (zip allObj allObj') []
  where
    allObj' = map (`goesTo` concat ops) allObj

t,s,e,b,n,w :: CyclePerm
t = [[S,W,N,E]]
s = [[E,B,W,T]]
e = [[B,S,T,N]]
b = [[N,W,S,E]]
n = [[W,B,E,T]]
w = [[T,S,B,N]]
