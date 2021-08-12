module CC where

type Amount = Integer
type Coin   = Integer
type Count  = Integer

cc :: Amount -> [Coin] -> Count
cc 0 _    = 1                   -- 金額が丁度 0 なら両替は 1 通り
cc _ []   = 0                   -- 両替に使う貨幣がなければ両替は 0 通り
cc a ccs@(c:cs)
  | a < 0 = 0                   -- 金額が0より少なければ両替は 0 通り
  | otherwise = cc (a-c) ccs    -- 最初の種類の貨幣紙面を引いた金額の両替の場合の数
              + cc a     cs     -- 最初の種類の紙幣以外を使う両替の場合の数
