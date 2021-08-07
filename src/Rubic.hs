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

e,e',s,s',w,w',n,n',t,t',b,b' :: [[Obj]]
e  = [[FR,UR,BR,DR],[RF,RU,RB,RD],[URF,BRU,DRB,FRD],[FUR,UBR,BDR,DFR],[RFU,RUB,RBD,RDF]]
s  = [[LF,UF,RF,DF],[FL,FU,FR,FD],[UFL,RFU,DFR,LFD],[LUF,URF,RDF,DLF],[FLU,FUR,FRD,FDL]]
w  = [[BL,UL,FL,DL],[LB,LU,LF,LD],[ULB,FLU,DLF,BLD],[BUL,UFL,FDL,DBL],[LBU,LUF,LFD,LDB]]
n  = [[RB,UB,LB,DB],[BR,BU,BL,BD],[UBR,LBU,DBL,RBD],[RUB,ULB,LDB,DRB],[BRU,BUL,BLD,BDR]]
t  = [[FU,LU,BU,RU],[UF,UL,UB,UR],[LUF,BUL,RUB,FUR],[FLU,LBU,BRU,RFU],[UFL,ULB,UBR,URF]]
b  = [[FD,RD,BD,LD],[DF,DR,DB,DL],[RDF,BDR,LDB,FDL],[FRD,RBD,BLD,LFD],[DFR,DRB,DBL,DLF]]
e' = prodPerm [e,e,e]
s' = prodPerm [s,s,s]
w' = prodPerm [w,w,w]
n' = prodPerm [n,n,n]
t' = prodPerm [t,t,t]
b' = prodPerm [b,b,b]

data Hand = R | R' | F | F' | L | L' | B | B' | U | U' | D | D' deriving (Show, Enum, Eq)

encode :: Hand -> [[Obj]]
encode R  = e
encode R' = e'
encode F  = s
encode F' = s'
encode L  = w
encode L' = w'
encode B  = n
encode B' = n'
encode U  = t
encode U' = t'
encode D  = b
encode D' = b'

isIdent :: [Hand] -> Bool
isIdent [R , R'] = True
isIdent [R', R ] = True
isIdent [F , F'] = True
isIdent [F', F ] = True
isIdent [L , L'] = True
isIdent [L', L ] = True
isIdent [B , B'] = True
isIdent [B', B ] = True
isIdent [U , U'] = True
isIdent [U', U ] = True
isIdent [D , D'] = True
isIdent [D', D ] = True
isIdent _        = False

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
  where g' = reverse $ map inverse g

inverse :: Hand -> Hand
inverse R  = R'
inverse R' = R
inverse F  = F'
inverse F' = F
inverse L  = L'
inverse L' = L
inverse B  = B'
inverse B' = B
inverse U  = U'
inverse U' = U
inverse D  = D'
inverse D' = D

-- Lemma

edgeCycle3 :: [Hand]
edgeCycle3 = [F, F, U, R', L, F, F, R, L', U, F, F]

edgeTranspose2 :: [Hand]
edgeTranspose2 = [U', B', D', F', R', F, D, B, U, R, F, D, B, U, R, U', B', D', F', R']

vertexCycle3 :: [Hand]
vertexCycle3 = [R, R, F, F, R', B', R, F, F, R', B, R']

vertexTwist2 :: [Hand]
vertexTwist2 = trans [R] (r ++ r)
  where r = [U, U, R, F', D, D, F, R']

diagTwist2 :: [Hand]
diagTwist2 = [R, R, B, U, B', U, B, U, U, B', U, U, B', U', B, U', B', U, U, B, U, U, R, R]

musubi :: [Hand]
musubi = [R, B, L, F, U, U, F', L', B', R', U, U]

vt2 :: [Hand]
vt2 = trans [B, B] diagTwist2

et3 :: [Hand]
et3 = [R, R, U, B', F, R, R, B, F', U, R, R]

et3bis :: [Hand]
et3bis = trans [U, B, R, F, D, U] et3
