module EditDistance where

import Control.Monad.State       (State, get, put, evalState)
import qualified Data.Map as Map (Map, lookup, insert, empty)

type Table k v = Map.Map k v
type Memo a b = State (Table a b) b

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
