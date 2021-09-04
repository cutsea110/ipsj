module Hilbert where

import Painter

hilbert :: Painter
hilbert = segmentsToPainter 8 8
          $ map Line
          [ [(2, 2), (2, 6), (6, 6), (6, 2)]
          , [(1, 1), (3, 1), (3, 3), (1, 3), (1, 7), (3, 7), (3, 5), (5, 5), (5, 7), (7, 7), (7, 3), (5, 3), (5, 1), (7, 1)]
          ]

main :: IO ()
main = withEPSHeader (squareLimit hilbert 3) unitSquare
