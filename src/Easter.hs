module Easter where

import Data.Time.Calendar.Month

easter :: Int -> (Month, Int)
easter y =
  if n'' > 31 then (MkMonth 4, n''-31) else (MkMonth 3, n'')
  where
    n'' = n' + 7 - (d + n') `mod` 7
      where n' = if n < 21 then n + 30 else n
            n = 44 - e'
    e' = if e == 25 && g > 11 || e == 24 then e + 1 else e
      where e = (11 * g + 20 + z - x) `mod` 30
    d = (5 * y) `div` 4 - x - 10
    x = 3 * c `div` 4 - 12
    z = (8 * c + 5) `div` 25 - 5
    c = y `div` 100 + 1
    g = y `mod` 19 + 1
    
