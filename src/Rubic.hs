module Rubic where

import Data.List

data Obj = TE | TS | TW | TN | ET | ES | EB | EN | ST | SE | SB | SW
         | BE | BS | BW | BN | WT | WS | WB | WN | NT | NE | NB | NW
         | TES | EST | STE | TSW | SWT | WTS | TWN | WNT | NTW | TNE
         | NET | ETN | BSE | SEB | EBS | BEN | ENB | NBE | BNW | NWB
         | WBN | BWS | WSB | SBW deriving (Eq, Enum, Show)

allObj :: [Obj]
allObj = [TE .. SBW]

goesTo :: Obj -> [[Obj]] -> Obj
goesTo c [] = c
goesTo c (s:ss)
  | length x == 0     = goesTo c ss
  | y == length s - 1 = goesTo (s!!0) ss
  | otherwise         = goesTo (s!!(y+1)) ss
    where
      [y] = x
      x   = elemIndices c s

assoc :: Obj -> [(Obj, Obj)] -> (Obj, Obj)
assoc c ((x, y):ss)
  | c == x    = (x, y)
  | otherwise = assoc c ss

makeCycle0 :: [(Obj, Obj)] -> [[Obj]] -> [[Obj]]
makeCycle0 [] qs = qs
makeCycle0 ((x, y):ss) qs
  | x == y    = makeCycle0 ss qs
  | otherwise = makeCycle1 ss ([x, y]:qs)

makeCycle1 :: [(Obj, Obj)] -> [[Obj]] -> [[Obj]]
makeCycle1 ss (cs:css)
  | c == head cs = makeCycle0 ss' (cs:css)
  | otherwise    = makeCycle1 ss' ((cs ++ [c]):css)
  where
    c   = snd d
    d   = assoc (last cs) ss
    ss' = delete d ss

prodPerm :: [[[Obj]]] -> [[Obj]]
prodPerm ops = makeCycle0 (zip allObj allGo) []
  where
    allGo = map (`goesTo` concat ops) allObj

e,e',s,s',w,w',n,n',t,t',b,b' :: [[Obj]]
e  = [[SE,TE,NE,BE],[ES,ET,EN,EB],[TES,NET,BEN,SEB],[STE,TNE,NBE,BSE],[EST,ETN,ENB,EBS]]
s  = [[WS,TS,ES,BS],[SW,ST,SE,SB],[TSW,EST,BSE,WSB],[WTS,TES,EBS,BWS],[SWT,STE,SEB,SBW]]
w  = [[NW,TW,SW,BW],[WN,WT,WS,WB],[TWN,SWT,BWS,NWB],[NTW,TSW,SBW,BNW],[WNT,WTS,WSB,WBN]]
n  = [[EN,TN,WN,BN],[NE,NT,NW,NB],[TNE,WNT,BNW,ENB],[ETN,TWN,WBN,BEN],[NET,NTW,NWB,NBE]]
t  = [[ST,WT,NT,ET],[TS,TW,TN,TE],[WTS,NTW,ETN,STE],[SWT,WNT,NET,EST],[TSW,TWN,TNE,TES]]
b  = [[SB,EB,NB,WB],[BS,BE,BN,BW],[EBS,NBE,WBN,SBW],[SEB,ENB,NWB,WSB],[BSE,BEN,BNW,BWS]]
e' = prodPerm [e,e,e]
s' = prodPerm [s,s,s]
w' = prodPerm [w,w,w]
n' = prodPerm [n,n,n]
t' = prodPerm [t,t,t]
b' = prodPerm [b,b,b]

data Hand = E | E' | S | S' | W | W' | N | N' | T | T' | B | B' deriving (Show, Enum, Eq)

encode :: Hand -> [[Obj]]
encode E  = e
encode E' = e'
encode S  = s
encode S' = s'
encode W  = w
encode W' = w'
encode N  = n
encode N' = n'
encode T  = t
encode T' = t'
encode B  = b
encode B' = b'

isIdent :: [Hand] -> Bool
isIdent [E , E'] = True
isIdent [E', E ] = True
isIdent [S , S'] = True
isIdent [S', S ] = True
isIdent [W , W'] = True
isIdent [W', W ] = True
isIdent [N , N'] = True
isIdent [N', N ] = True
isIdent [T , T'] = True
isIdent [T', T ] = True
isIdent [B , B'] = True
isIdent [B', B ] = True
isIdent _        = False

seqs :: Int -> [[Hand]]
seqs 1 = [[x] | x <- [E .. B']]
seqs n = [ x:yys
         | x <- [E .. B']
         , yys@(y:ys) <- seqs (n-1)
         , not $ isIdent [x, y]
         ]

-- Lemma

edgeCycle3 :: [[Obj]]
edgeCycle3 = prodPerm $ map encode [S, S, T, E', W, S, S, E, W', T, S, S]

edgeTranspose2 :: [[Obj]]
edgeTranspose2 = prodPerm $ map encode [T', N', B', S', E', S, B, N, T, E, S, B, N, T, E, T', N', B', S', E']

vertexCycle3 :: [[Obj]]
vertexCycle3 = prodPerm $ map encode [E, E, S, S, E', N', E, S, S, E', N, E']

vertexTwist2 :: [[Obj]]
vertexTwist2 = prodPerm $ map encode $ [E] ++ r ++ r ++ [E']
  where r = [T, T, E, S', B, B, S, E']

diagTwist2 :: [[Obj]]
diagTwist2 = prodPerm $ map encode [E, E, N, T, N', T, N, T, T, N', T, T, N', T', N, T', N', T, T, N, T, T, E, E]

vt2 :: [[Obj]]
vt2 = prodPerm $ map encode $  [N, N] ++ [E, E, N, T, N', T, N, T, T, N', T, T, N', T', N, T', N', T, T, N, T, T, E, E] ++ [N', N']

et3 :: [[Obj]]
et3 = prodPerm $ map encode [E, E, T, N', S, E, E, N, S', T, E, E]

et3bis :: [[Obj]]
et3bis = prodPerm $ map encode $ [T, N, E, S, B, T] ++ [E, E, T, N', S, E, E, N, S', T, E, E] ++ [T', B', S', E', N', T']

-- t : 緑
-- b : 黄
-- n : 赤
-- s : 橙
-- e : 青
-- w : 白
