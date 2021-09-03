module RhinecanthusRectangulus where

import Painter

fish = segmentsToPainter 150 150
       [[(6,43),(13,50),(38,63),(50,69),(60,73),(72,69),(98,56),(104,52),(114,46) -- 本体上側
        ,(140,60),(138,58),(139,51),(140,41),(140,38),(132,23),(132,18),(112,33) -- 尾鰭
        ,(106,31),(98,28),(80,20),(70,16),(15,36),(6,43)] -- 本体下側
       ,[(118,42),(135,49)],[(117,38),(130,38)],[(117,34),(129,30)] -- 尾鰭
       ,[(13,50),(22,40),(15,36)],[(6,43),(22,40)] -- 口
       ,[(49,55),(50,57),(51,58),(53,58),(54,57),(55,55),(54,53),(53,52),(51,52),(50,53),(49,55)] -- 目
       ,[(72,69),(85,80),(87,80),(90,79),(100,74),(110,64),(114,46)],[(80,66),(90,79)],[(90,62),(100,74)],[(102,56),(110,64)] -- 背鰭
       ,[(80,20),(87,9),(91,8),(100,14),(105,18),(110,27),(112,33)],[(86, 20),(91,8)],[(96,22),(100,14)],[(102,24),(105,18)] -- 腹鰭
       ,[(60,31),(64,39),(72,38),(74,32),(70,29),(60,31)] -- 胸鰭
       ,[(38,63),(60,31),(80,20)]
       ,[(50,69),(71,40),(98,28)]
       ,[(71,40),(98,56)]
       ,[(104,52),(90,40),(106,31)]
       ,[(114,46),(112,33)]
       ]

main :: IO ()
main = fish unitSquare
