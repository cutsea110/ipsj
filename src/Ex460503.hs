module Ex460503 where

import Data.List (sort, group, nub)
import Ticket

-- perm を使って作成したものから重複を除去
gperm :: Eq a => [a] -> [[a]]
gperm = nub . perm

-- 最初から重複したものを作成しない
gperm' :: Ord a => [a] -> [[a]]
gperm' xs = foldr (concatMap . merges) [[]] (group (sort xs))

merges :: [a] -> [a] -> [[a]]
merges [] ys = [ys]
merges xs [] = [xs]
merges xxs@(x:xs) yys@(y:ys)
  = map (x:) (merges xs yys) ++ map (y:) (merges xxs ys)
