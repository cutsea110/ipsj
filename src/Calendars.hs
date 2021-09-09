module Calendars
  ( cals
  ) where

import Calendar (month, cc)

cals :: Int -> IO ()
cals y = do
  putStrLn (replicate 30 ' ' ++ show y ++ "\n")
  putStrLn (unlines (map unwords mcc))
    where mcc = concatMap mc3 [1,4,7,10]
          mc3 m = map f (zip3 (mc m) (mc (m+1)) (mc (m+2)))
            where f (a, b, c) = [a, b, c]
          mc m = month m : cc y m
