module Mark
  ( Mark(..)
  , isEmpty
  , next
  ) where

data Mark = O
          | X
          | U
          | B
          deriving Eq

instance Show Mark where
  show O = "O"
  show X = "X"
  show U = "."
  show B = "*"

isEmpty :: Mark -> Bool
isEmpty = (U==)

next :: Mark -> Mark
next O = X
next X = O
