module Factorial where

factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
