module MemoCC where

type Amount = Integer
type Coin   = Integer
type Count  = Integer

type Table = [(Key, Value)]
type Key   = (Amount, [Coin])
type Value = Count

emptyTable :: Table
emptyTable = []

lookupTable :: Key -> Table -> [Value]
lookupTable key []          = []
lookupTable key ((k,v):tbl) | key > k  = []
                            | key == k = [v]
                            | key < k  = lookupTable key tbl

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
