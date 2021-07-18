module DumbTree where

data DumbTree = Empty | Fork DumbTree DumbTree
{-
instance Show DumbTree where
  show Empty      = "O"
  show (Fork l r) = "(" ++ show l ++ "^" ++ show r  ++ ")"
-}
instance Show DumbTree where
  show = showLR "C"

showLR :: String -> DumbTree -> String
showLR s Empty      = s
showLR s (Fork l r) = "("++showLR "L" l++"^"++showLR "R" r++")"
    

trees :: Int -> [DumbTree]
trees 1 = [Empty]
trees n = concat [ joins ls rs | (ls, rs) <- [ lrs xs ys | (xs, ys) <- splits1 n ]]

splits1 :: Int -> [(Int, Int)]
splits1 1 = []
splits1 n = (1, n-1) : [ (i+1, j) | (i, j) <- splits1 (n-1) ]

lrs :: Int -> Int -> ([DumbTree], [DumbTree])
lrs xs ys = (trees xs, trees ys)

joins :: [DumbTree] -> [DumbTree] -> [DumbTree]
joins ls rs = [ Fork l r | l <- ls, r <- rs ]
