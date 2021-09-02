module CrowdOfFunctionalProgrammers where

import Painter

man :: Painter
man = segmentsToPainter 20 20
    [[ (6, 10), (0, 10), (0, 12), (6, 12), (6, 14), (4, 16), (4, 18), (6, 20), (8, 20), (10, 18)
     , (10, 16), (8, 14), (8, 12), (10, 12), (10, 14), (12, 14), (12, 10), (8, 10), (8, 8)
     , (10, 0), (8, 0), (7, 4), (6, 0), (4, 0), (6, 8), (6, 10)]
    ]
    

horiz :: Float -> Painter -> Painter
horiz 0 _ = blank
horiz n p = beside 1 (n-1) p (horiz (n-1) p)


crowd :: Painter -> Painter
crowd p = above (1-s10) s10 blank
          (above (inv 10) s9 (horiz 10 p)
           (above (inv 9) s8 (horiz 9 p)
            (above (inv 8) s7 (horiz 8 p)
             (above (inv 7) s6 (horiz 7 p)
              (above (inv 6) (inv 5) (horiz 6 p) (horiz 5 p))))))
  where inv n = fromRational (1/n)
        s10 = sum (map inv [5..10])
        s9 = sum (map inv [5..9])
        s8 = sum (map inv [5..8])
        s7 = sum (map inv [5..7])
        s6 = sum (map inv [5..6])
    
main :: IO ()
main = crowd man unitSquare
