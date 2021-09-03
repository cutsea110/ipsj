module RhinecanthusRectangulus where

import Painter

fish = segmentsToPainter 150 150
       [[(4,45),(13,50),(38,63),(50,69),(60,73),(72,69),(98,56),(104,52),(114,46),(138,52),(136,25),(112,33),(106,31),(98,28),(80,20),(70,16),(15,36),(4,45)] -- 体
       ,[(13,50),(22,40),(15,36)] -- 口
       ,[(49,55),(50,57),(51,58),(53,58),(54,57),(55,55),(54,53),(53,52),(51,52),(50,53),(49,55)] -- 目
       ,[(72,69),(85,80),(87,80),(114,46)] -- 背鰭
       ,[(80,20),(87,9),(90,8),(112,33)] -- 腹鰭
       ,[(38,63),(60,31),(80,20)]
       ,[(50,69),(71,40),(98,28)]
       ,[(71,40),(98,56)]
       ,[(104,52),(90,40),(106,31)]
       ,[(114,46),(112,33)]
       ]

main :: IO ()
main = fish unitSquare
