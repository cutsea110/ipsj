module EditDistance where

import Control.Monad.State       (State, get, put, evalState)
import Data.List (transpose)
import qualified Data.Map as Map (Map, lookup, insert, empty)

type Table k v = Map.Map k v
type Memo a b = State (Table a b) b

data OpC = Ins Char | Del Char | Subst Char Char | Keep Char
type IntOps = (Int, [OpC])

instance Show OpC where
  show (Ins c)      = ['-', 'v',  c ]
  show (Del c)      = [ c,  '^', '-']
  show (Subst c d ) = [ c,  '!',  d ]
  show (Keep c)     = [ c,  ' ',  c ]

memoise :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoise f x = do
  table <- get
  case Map.lookup x table of
    Just y  -> return y
    Nothing -> do fx <- f x
                  table' <- get
                  put (Map.insert x fx table')
                  return fx

runM :: (a -> Memo a b) -> a -> b
runM m v = evalState (m v) Map.empty

edM :: (String, String) -> Memo (String, String) IntOps
edM ([],       []      ) = return (0, [])
edM (xs@(_:_), []      ) = return (length xs, map Del xs)
edM ([],       ys@(_:_)) = return (length ys, map Ins ys)
edM xys@(_:_,   _:_    ) = memoise edM' xys
  where
    edM' (xxs@(x:xs), yys@(y:ys)) = do
      (a, ops_a) <- edM (xs,  ys)
      (b, ops_b) <- edM (xxs, ys)
      (c, ops_c) <- edM (xs,  yys)
      return (min3
               (if x == y
                 then (a, Keep x:ops_a)
                 else (1+a, Subst x y:ops_a))
               (1+b, Ins y:ops_b)
               (1+c, Del x:ops_c))

min3 :: Ord a => (a, b) -> (a, b) -> (a, b) -> (a, b)
min3 a b c = if a `le` b then a `min2` c else b `min2` c
  where
    (x, _) `le` (y, _) = x <= y
    min2 x y | x `le` y  = x
             | otherwise = y

edMemo :: (String, String) -> IntOps
edMemo = runM edM

edAll :: (String, String) -> IO ()
edAll xys = do
  putStrLn $ show d
  mapM_ putStrLn $ transpose $ map show ops
  where
    (d, ops) = edMemo xys
