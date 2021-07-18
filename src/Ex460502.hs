module Ex460502 where

import Data.List
import Ticket hiding (ticket)

ticket :: Int -> [Char] -> Term
ticket n ds
  = case filter (same n) (allterms ds) of
      s:_ -> s
      []  -> error ("Cannot make " ++ show n ++ " with " ++ intersperse ',' ds ++ ".")
