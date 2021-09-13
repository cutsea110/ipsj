module SolarLongitude where

import JulianDate (julianDate) -- for test

sind d = sin (d * pi / 180) -- 度単位の三角関数
cosd d = cos (d * pi / 180)
tand d = tan (d * pi / 180)
datan x = 180 / pi * atan x

ld d = norm (280.4664 + 0.98564736 * d + 0.0003 * t * t) 360 -- 平均黄軽 l(d)
  where t = d / 36525
omegad d = 282.9373 + 0.00004708 * d + 0.0005 * t * t -- 近地点黄軽 omega(d)
  where t = d / 36525
ed d = 0.016709 - 0.000042 * t -- 離心率 e(d)
  where t = d / 36525

norm :: Float -> Integer -> Float -- 0 ^ 360 度に正規化
norm a d = fromInteger (b `mod` d) + c
  where (b, c) = properFraction a  -- b, c は a の整数部と小数部
lon td                -- td(JD) における太陽黄経を計算
  = norm l 360        -- julian date of the day time
  where d = td - 2451545.0           -- 2000年1月1日正午(UT)からの日数
        e = ed d                     -- その時点での離心率
        omega = omegad d             -- その時点での近地点黄経
        m = ld d - omega             -- 平均近点離角
        e' = kepler e m              -- 離心近点離角
        kepler e m = f e0            -- Kepler 方程式を解く
          where m' = m * pi / 180    -- 弧度法に直す
                e0 = m'              -- Newton 方の初期値
                f e0 = if abs (e0 - e1) < 0.00001 then e1 * 180 / pi else f e1
                  where e1 = e0 - (e0 - e * sin e0 - m') / (1 - e * cos e0)
        l = v e m + omega            -- 太陽黄経
          where v e m = 2 * datan (sqrt ((1 + e) / (1 - e)) * tand (e' / 2))

----
-- 理科年表では 278.825
-- この値は     278.88284 で誤差あり.
test :: Float
test = lon (fromIntegral (julianDate 2004 1 0) - 0.5)
