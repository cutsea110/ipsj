module CrowdOfFunctionalProgrammers where

import Painter

man :: Painter
man = segmentsToPainter 20 20
      [[ (6, 10), (0, 10), (0, 12), (6, 12), (6, 14), (4, 16), (4, 18), (6, 20), (8, 20), (10, 18)
       , (10, 16), (8, 14), (8, 12), (10, 12), (10, 14), (12, 14), (12, 10), (8, 10), (8, 8), (10, 0)
       , (8, 0), (7, 4), (6, 0), (4, 0), (6, 8), (6, 10)]
      ]

horiz :: Float -> Painter -> Painter
horiz 0 _ = blank
horiz n p = beside 1 (n-1) p (horiz (n-1) p)

crowd :: [Float] -> Painter -> Painter
crowd xs p = foldl f blank $ zip3 (rs++[rest]) (0:accs) (ps++[blank])
  where inv n = 1/n
        rs = map inv xs
        accs = scanl1 (+) rs
        (ttl, rest) = (last accs, 1 - ttl)
        ps = map (`horiz` p) xs
        f q (r, s, p) = above r s p q

main :: IO ()
main = crowd [5..10] man unitSquare
