module JulianDate where

mon0 :: [Int]
mon0 = scanl (+) 0 [31,28,31,30,31,30,31,31,30,31,30] -- 平年
mon1 :: [Int]
mon1 = scanl (+) 0 [31,29,31,30,31,30,31,31,30,31,30] -- うるう年

-- | Gregorian 暦のうるう年に True を返す
gleap :: Int -> Bool
gleap y = if y `mod` 100 == 0 then y `mod` 400 == 0 else y `mod` 4 == 0

-- | Julian 暦のうるう年に True を返す
jleap :: Int -> Bool
jleap y = y `mod` 4 == 0

julianDate :: Int -> Int -> Int -> Int
julianDate y m d =
  let a = (y+4712) * 365 -- すべて平年とした場合の前年末までの総日数
      b = (y+4712+3) `div` 4 -- それまでの 4 で割れる年の総数
                             -- 3足して4で割るのは大きい方に丸める常套手段
      c = if y > 1601  -- 1601 年以降について 100年 400 年の補正をする項
          then y' `div` 400 - y' `div` 100 -- b だと足しすぎだから
          else 0                           -- 別名 inclusion and exclusion prinsiple
        where y' = y - 1601
      e = if [y,m,d] >= [1582,10,15] then -10 else 0 -- 1582年の改暦で飛ばした10日を減ずる
      f = (if leap y then mon1 else mon0) !! (m-1)  -- その年内で前月までの日数を得る
        where leap = if y > 1600 then gleap else jleap
      g = d - 1 -- 1月1日の Julian Date が 0 だから(julianDate (-4712) 1 1 => 0)
  in a + b + c + e + f + g
