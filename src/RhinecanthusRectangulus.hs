module RhinecanthusRectangulus where

import Painter

humuhumu :: Painter
humuhumu
  = segmentsToPainter 150 150
    [[(7,43),(13,50),(38,63),(50,69),(60,73),(72,69),(98,56),(104,52),(114,46) -- 本体上側
     ,(140,60),(138,58),(139,51),(140,41),(140,38),(132,23),(132,18),(112,33) -- 尾鰭
     ,(106,31),(98,28),(80,20),(70,16),(55,18),(15,36),(7,43)] -- 本体下側
    ,[(118,42),(135,49)],[(117,38),(130,38)],[(117,34),(129,30)] -- 尾鰭
    ,[(13,50),(22,40),(15,36)],[(7,43),(22,40)] -- 口
    ,[(49,56),(51,58),(53,58),(55,56),(55,54),(53,52),(51,52),(49,54),(49,56)] -- 目
    ,[(48,57),(50,59),(54,59),(56,57),(56,53),(54,51),(50,51),(48,53),(48,57)] -- 目
    ,[(72,69),(85,80),(87,80),(90,79),(100,74),(110,64),(114,46)],[(80,66),(90,79)],[(90,62),(100,74)],[(102,56),(110,64)] -- 背鰭
    ,[(80,20),(87,9),(91,8),(100,14),(105,18),(110,27),(112,33)],[(86, 20),(91,8)],[(96,22),(100,14)],[(102,24),(105,18)] -- 腹鰭
    ,[(60,31),(64,39),(72,38),(74,32),(70,29),(60,31)] -- 胸鰭
    ,[(38,63),(60,31),(80,20)]   -- 目の下の柄
    ,[(50,69),(71,40),(98,28)]   -- 目の上の柄
    ,[(71,40),(98,56)]           -- ラムダの上の柄
    ,[(104,52),(90,40),(106,31)] -- ラムダの股の柄
    ,[(114,46),(112,33)]         -- 尾の付け根の柄
    ,[(8,56),(9,57),(10,56),(9,55),(8,56)] -- 小さい泡
    ,[(10,61),(11,62),(12,62),(13,61),(13,60),(12,59),(11,59),(10,60),(10,61)] -- 中くらいの泡
    ,[(12,70),(14,72),(16,72),(18,70),(18,68),(16,66),(14,66),(12,68),(12,70)] -- 大きい泡
    ]

main :: IO ()
main = humuhumu unitSquare
