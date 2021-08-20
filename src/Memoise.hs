module Memoise where

type Amount = Integer
type Coin   = Integer
type Count  = Integer

type State s t = s -> (t, s)

-- | return
withState :: t -> State s t
withState x = \state -> (x, state)

-- | >>=
bindState :: State s t -> (t -> State s u) -> State s u
bindState sx sf s0 = let (x, s1) = sx s0 in sf x s1

evalState :: State s t -> s -> t
evalState s s0 = fst (s s0)

fun1WithState :: (a -> b) -> State s a -> State s b
fun1WithState f sx = bindState sx (\x -> withState (f x))
fun2WithState :: (a -> b -> c) -> State s a -> State s b -> State s c
fun2WithState f sx sy = bindState sx (\x -> bindState sy (\y -> withState (f x y)))

type Table = [(Key, Value)]
type Key   = (Amount, [Coin])
type Value = Count

emptyTable :: Table
emptyTable = []

lookupTable :: Key -> Table -> [Value]
lookupTable key []          = []
lookupTable key ((k,v):tbl) | key >  k = []
                            | key == k = [v]
                            | key <  k = lookupTable key tbl

insertTable :: Key -> Value -> Table -> Table
insertTable k v tbl =
  case break ((k >) . fst) tbl of
       (xs, ys) -> xs ++ (k, v):ys

memocc :: Amount -> [Coin] -> Table -> (Count, Table)
memocc 0 _  tbl = (1, tbl)
memocc _ [] tbl = (0, tbl)
memocc a ccs@(c:cs) tbl
  | a < 0     = (0, tbl)
  | otherwise = case lookupTable (a, ccs) tbl of
                  (v:_) -> (v, tbl)
                  []    -> let (cnt1, tbl1) = memocc (a-c) ccs tbl
                               (cnt2, tbl2) = memocc a     cs  tbl1
                               cnt3         = cnt1 + cnt2
                               tbl3         = insertTable (a, ccs) cnt3 tbl2
                           in (cnt3, tbl3)

evalMemoCC :: Amount -> [Coin] -> Count
evalMemoCC amount coins = fst (memocc amount coins emptyTable)
