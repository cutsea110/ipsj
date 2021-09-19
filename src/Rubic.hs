module Rubic where

import Data.List

data Obj = UR | UF | UL | UB | RU | RF | RD | RB | FU | FR | FD | FL
         | DR | DF | DL | DB | LU | LF | LD | LB | BU | BR | BD | BL
         | URF | RFU | FUR | UFL | FLU | LUF | ULB | LBU | BUL | UBR
         | BRU | RUB | DFR | FRD | RDF | DRB | RBD | BDR | DBL | BLD
         | LDB | DLF | LFD | FDL deriving (Eq, Enum, Show)

allObj :: [Obj]
allObj = [UR .. FDL]

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

r,r',f,f',l,l',b,b',u,u',d,d' :: [[Obj]]
r  = [[FR,UR,BR,DR],[RF,RU,RB,RD],[URF,BRU,DRB,FRD],[FUR,UBR,BDR,DFR],[RFU,RUB,RBD,RDF]]
f  = [[LF,UF,RF,DF],[FL,FU,FR,FD],[UFL,RFU,DFR,LFD],[LUF,URF,RDF,DLF],[FLU,FUR,FRD,FDL]]
l  = [[BL,UL,FL,DL],[LB,LU,LF,LD],[ULB,FLU,DLF,BLD],[BUL,UFL,FDL,DBL],[LBU,LUF,LFD,LDB]]
b  = [[RB,UB,LB,DB],[BR,BU,BL,BD],[UBR,LBU,DBL,RBD],[RUB,ULB,LDB,DRB],[BRU,BUL,BLD,BDR]]
u  = [[FU,LU,BU,RU],[UF,UL,UB,UR],[LUF,BUL,RUB,FUR],[FLU,LBU,BRU,RFU],[UFL,ULB,UBR,URF]]
d  = [[FD,RD,BD,LD],[DF,DR,DB,DL],[RDF,BDR,LDB,FDL],[FRD,RBD,BLD,LFD],[DFR,DRB,DBL,DLF]]
r' = prodPerm [r,r,r]
f' = prodPerm [f,f,f]
l' = prodPerm [l,l,l]
b' = prodPerm [b,b,b]
u' = prodPerm [u,u,u]
d' = prodPerm [d,d,d]

data Hand = R | R' | F | F' | L | L' | B | B' | U | U' | D | D' deriving (Show, Enum, Eq)

encode :: Hand -> [[Obj]]
encode R  = r
encode R' = r'
encode F  = f
encode F' = f'
encode L  = l
encode L' = l'
encode B  = b
encode B' = b'
encode U  = u
encode U' = u'
encode D  = d
encode D' = d'

isIdent :: [Hand] -> Bool
isIdent [x, y] = prime x == y

seqs :: Int -> [[Hand]]
seqs 1 = [[x] | x <- [R .. D']]
seqs n = [ x:yys
         | x <- [R .. D']
         , yys@(y:ys) <- seqs (n-1)
         , not $ isIdent [x, y]
         ]

run :: [Hand] -> [[Obj]]
run = prodPerm . map encode

trans :: [Hand] -> [Hand] -> [Hand]
trans g f = g ++ f ++ g'
  where g' = reverse $ map prime g

prime :: Hand -> Hand
prime R  = R'
prime R' = R
prime F  = F'
prime F' = F
prime L  = L'
prime L' = L
prime B  = B'
prime B' = B
prime U  = U'
prime U' = U
prime D  = D'
prime D' = D

inverse :: [Hand] -> [Hand]
inverse = reverse . map prime

-- Lines of the Palm
m :: [Hand]
m = [R', L]

trigger :: [Hand]
trigger = [R, U, R']

jackKhife :: [Hand]
jackKhife = [R, U', R']

snake :: [Hand]
snake = [R', F, R]

sledgeHammer :: [Hand]
sledgeHammer = [R', F, R, F']

-- Lemma

{- |
>>> run edgeCycle3
[[RU,FU,LU],[UR,UF,UL]]
-}
edgeCycle3 :: [Hand]
edgeCycle3 = [F, F, U, R', L, F, F, R, L', U, F, F]

{- |
>>> run edgeTranspose2
[[UF,FU],[UR,RU]]
-}
edgeTranspose2 :: [Hand]
edgeTranspose2 = [U', B', D', F', R', F, D, B, U, R, F, D, B, U, R, U', B', D', F', R']

{- |
>>> run vertexCycle3
[[FUR,LUF,RUB],[RFU,FLU,BRU],[URF,UFL,UBR]]
-}
vertexCycle3 :: [Hand]
vertexCycle3 = [R, R, F, F, R', B', R, F, F, R', B, R']

{- |
>>> run vertexTwist2
[[UFL,LUF,FLU],[URF,RFU,FUR]]
-}
vertexTwist2 :: [Hand]
vertexTwist2 = trans [R] (r ++ r)
  where r = [U, U, R, F', D, D, F, R']

{- |
>>> run diagTwist2
[[DRB,BDR,RBD],[UFL,FLU,LUF]]
-}
diagTwist2 :: [Hand]
diagTwist2 = [R, R, B, U, B', U, B, U, U, B', U, U, B', U', B, U', B', U, U, B, U, U, R, R]

{- |
>>> run musubi
[[FU,BU,FR],[UF,UB,RF]]
-}
musubi :: [Hand]
musubi = [R, B, L, F, U, U, F', L', B', R', U, U]

{- |
>>> run vt2
[[ULB,BUL,LBU],[UFL,FLU,LUF]]
-}
vt2 :: [Hand]
vt2 = trans [B, B] diagTwist2

{- |
>>> run ec3
[[RU,FU,BU],[UR,UF,UB]]
-}
ec3 :: [Hand]
ec3 = [R, R, U, B', F, R, R, B, F', U, R, R]

{- |
>>> run ec3bis
[[RF,FU,BR],[UF,RB,FR]]
-}
ec3bis :: [Hand]
ec3bis = trans [U, B, R, F, D, U] ec3
