module Lcs where

import Memoise

lcs :: String -> String -> (Integer, String)
lcs "" _  = (0, "")
lcs _  "" = (0, "")
lcs xxs@(x:xs) yys@(y:ys)
  = if x == y
    then cons x (lcs xs ys)
    else maxlen (lcs xs yys) (lcs xxs ys)

cons :: Char -> (Integer, String) -> (Integer, String)
cons x (ls, xs) = (ls+1, x:xs)

maxlen :: (Integer, String) -> (Integer, String) -> (Integer, String)
maxlen xs@(ls, _) ys@(ly, _) = if ls > ly then xs else ys

memolcs :: Memo (String, String) (Integer, String)
memolcs ("", _)  = withState (0, "")
memolcs (_,  "") = withState (0, "")
memolcs xxsyys
  = memoise (\(xxs@(x:xs), yys@(y:ys)) ->
               if x == y
               then consS (withState x) (memolcs (xs, ys))
               else maxlenS (memolcs (xs, yys)) (memolcs (xxs, ys))
            ) xxsyys
    where
      consS = fun2WithState cons
      maxlenS = fun2WithState  maxlen

evalMemoLCS :: String -> String -> (Integer, String)
evalMemoLCS xs ys = evalState (memolcs (xs, ys)) emptyTable

