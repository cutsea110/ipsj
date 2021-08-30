{-# LANGUAGE NPlusKPatterns #-}
module Hilbert where

left, right :: (Int, Int) -> (Int, Int)
left  (dx, dy) = (-dy, dx) -- 左折
right (dx, dy) = (dy, -dx) -- 右折

-- | F に対応する, rlineto は相対位置まで線を引く
f :: (Int, Int) -> IO ()
f (dx, dy) = putStrLn $ show dx ++ " " ++ show dy ++ " rlineto"

-- | X, Y に対応
x, y :: Int -> (Int, Int) -> IO ()
x 0     (_, _)   = putStr "" -- 0次なら何もしない. 空を出力
x (n+1) (x0, y0) = do
  y n (x1, y1); f (x1, y1); x n (x0, y0); f (x0, y0)
  x n (x0, y0); f (x3, y3); y n (x3, y3)
    where (x1, y1) = left (x0, y0)
          (x3, y3) = right (x0, y0)

y 0     (_, _)   = putStr ""
y (n+1) (x0, y0) = do
  x n (x3, y3); f (x3, y3); y n (x0, y0); f (x0, y0)
  y n (x0, y0); f (x1, y1); x n (x1, y1)
    where (x1, y1) = left (x0, y0)
          (x3, y3) = right (x0, y0)

-- | ドライバ
hilbert :: Int -> Int -> IO ()
hilbert size n = do
  putStrLn $ show o ++ " " ++ show o ++ " moveto"
  x n (x0, y0)
  putStrLn "stroke"
    where x0 = size `div` (2^n) -- ^ セグメント長 (- 移動距離)
          y0 = 0
          o = x0 `div` 2        -- ^ 原点から出発位置までの距離

main :: IO ()
main = hilbert 256 5
