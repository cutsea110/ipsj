{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Memoise where

type Amount = Integer
type Coin   = Integer
type Count  = Integer

type State s t = s -> (t, s)

-- | return
withState :: t -> State s t
withState x state = (x, state)

-- | >>=
bindState :: State s t -> (t -> State s u) -> State s u
bindState sx sf s0 = let (x, s1) = sx s0 in sf x s1

evalState :: State s t -> s -> t
evalState s s0 = fst (s s0)

fun1WithState :: (a -> b) -> State s a -> State s b
fun1WithState f sx = bindState sx (withState . f)
fun2WithState :: (a -> b -> c) -> State s a -> State s b -> State s c
fun2WithState f sx sy = bindState sx (\x -> bindState sy (withState . f x))

memoise :: Ord a => (a -> State (Table a b) b) -> a -> State (Table a b) b
memoise f x tbl = case lookupTable x tbl of
  y:_ -> (y, tbl)
  []  -> let (y, tbl') = f x tbl
         in (y, insertTable x y tbl')

type Table a b = [(a, b)]
type Key   = (Amount, [Coin])
type Value = Count

emptyTable :: Table a b
emptyTable = []

lookupTable :: Ord a => a -> Table a b -> [b]
lookupTable key []          = []
lookupTable key ((k,v):tbl) | key >  k = []
                            | key == k = [v]
                            | key <  k = lookupTable key tbl

insertTable :: Ord a => a -> b -> Table a b -> Table a b
insertTable k v tbl =
  case break ((k >) . fst) tbl of
       (xs, ys) -> xs ++ (k, v):ys

memocc :: Key -> State (Table Key Value) Value
memocc (0, _ ) = 1
memocc (_, []) = 0
memocc arg@(a, _)
  | a < 0 = 0
  | otherwise = memoise (\(a, ccs@(c:cs)) -> memocc (a-c, ccs) + memocc (a, cs)) arg

evalMemoCC :: Amount -> [Coin] -> Count
evalMemoCC amount coins = fst (memocc (amount, coins) emptyTable)

instance Eq b => Eq (State (Table a b) b) where
  sx == sy = evalState sx emptyTable == evalState sy emptyTable

instance Show b => Show (State (Table a b) b) where
  show sx = show (evalState sx emptyTable)

instance Num b => Num (State (Table a b) b) where
  (+)         = fun2WithState (+)
  (-)         = fun2WithState (-)
  (*)         = fun2WithState (*)
  negate      = fun1WithState negate
  abs         = fun1WithState abs
  signum      = fun1WithState signum
  fromInteger = withState . fromInteger
