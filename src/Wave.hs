module Wave where

import Painter

wave :: Painter
wave = segmentsToPainter 20 20
       $ map Line
       [ [(0, 13), (3, 8), (6, 12), (7, 11), (5, 0)], [(8, 0), (10, 6), (12, 0)]
       , [(15, 0), (12, 10), (20, 3)], [(20, 7), (15, 13), (12, 13), (13, 17), (12, 20)]
       , [(8, 20), (7, 17), (8, 13), (6, 13), (3, 12), (0, 17)]
       ]

main :: IO ()
main = withEPSHeader (squareLimit wave 3) unitSquare
