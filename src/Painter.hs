{-# LANGUAGE NPlusKPatterns #-}
module Painter where

type Vect = (Float, Float)
type LineSegment = [Vect]
type Figure = [LineSegment]
type Frame = (Vect, Vect, Vect)
type Painter = Frame -> IO ()

blank :: Painter
blank = \frame -> putStr ""

infixr 7 +~, -~
infixr 8 *~
(+~) :: Vect -> Vect -> Vect
(x0, y0) +~ (x1, y1) = (x0+x1, y0+y1)

(-~) :: Vect -> Vect -> Vect
(x0, y0) -~ (x1, y1) = (x0-x1, y0-y1)
(*~) :: Float -> Vect -> Vect
a *~ (x, y) = (a*x, a*y)

drawLine :: [Vect] -> String
drawLine ((x, y):xys) = show x ++ " " ++ show y ++ " moveto\n" ++ drawLine' xys

drawLine' :: [Vect] -> String
drawLine' [] = ""
drawLine' ((x, y):xys) = show x ++ " " ++ show y ++ " lineto\n" ++ drawLine' xys

segmentsToPainter :: Float -> Float -> Figure -> Painter
segmentsToPainter scale0 scale1 segs =
  \frame -> putStr $
            let toFrame (x, y) = frameCoodMap frame (x/scale0, y/scale1)
                drawSeg seg = drawLine (map toFrame seg)
            in concat (map drawSeg segs) ++ "stroke\n"

frameCoodMap :: Frame -> (Vect -> Vect)
frameCoodMap (org, edge0, edge1) =
  \(x, y) -> org +~ x *~ edge0 +~ y *~ edge1

transformPainter :: Painter -> Vect -> Vect -> Vect -> Painter
transformPainter painter org edge0 edge1 =
  \frame -> let m = frameCoodMap frame
                newOrg = m org
                newEdge0 = m edge0 -~ newOrg
                newEdge1 = m edge1 -~ newOrg
            in painter (newOrg, newEdge0, newEdge1)

rot :: Painter -> Painter
rot p = transformPainter p (1, 0) (1, 1) (0, 0)
flipHoriz :: Painter -> Painter
flipHoriz p = transformPainter p (1, 0) (0, 0) (1, 1)
flipVert :: Painter -> Painter
flipVert p = transformPainter p (0, 1) (1, 1) (0, 0)

above :: Float -> Float -> Painter -> Painter -> Painter
above m n p q =
  \frame -> do transformPainter p (0, r) (1, r) (0, 1) frame
               transformPainter q (0, 0) (1, 0) (0, r) frame
                 where r = n/(m+n)

beside :: Float -> Float -> Painter -> Painter -> Painter
beside m n p q =
  \frame -> do transformPainter p (0, 0) (r, 0) (0, 1) frame
               transformPainter q (r, 0) (1, 0) (r, 1) frame
                 where r = m/(m+n)

infixr 3 </>
infixr 4 <->
(</>) :: Painter -> Painter -> Painter
(</>) = above 1 1
(<->) :: Painter -> Painter -> Painter
(<->) = beside 1 1

unitSquare :: Frame
unitSquare = ((128, 16), (256, 0), (0, 256))

cornerSplit :: Painter -> Int -> Painter
cornerSplit _ 0 = blank
cornerSplit p (n+1) = (topLeft </> p) <-> (corner </> bottomRight)
  where up = upSplit p n
        right = rightSplit p n
        topLeft = up <-> up
        bottomRight = right </> right
        corner = cornerSplit p n

rightSplit :: Painter -> Int -> Painter
rightSplit _ 0 = blank
rightSplit p (n+1) = p <-> (smaller </> smaller)
  where smaller = rightSplit p n

upSplit :: Painter -> Int -> Painter
upSplit _ 0 = blank
upSplit p (n+1) = (smaller <-> smaller) </> p
  where smaller = upSplit p n

squareLimit :: Painter -> Int -> Painter
squareLimit p n = half </> flipVert half
  where half = flipHoriz quarter <-> quarter
        quarter = cornerSplit p n

-- | 左上を中心に 45 度の回転
rot45 :: Painter -> Painter
rot45 p = transformPainter p (0.5, 0.5) (1, 1) (0, 1)

-- | 2 枚のペインタを重ねる
over :: Painter -> Painter -> Painter
over p q = \frame -> do
  transformPainter p (0, 0) (1, 0) (0, 1) frame
  transformPainter q (0, 0) (1, 0) (0, 1) frame

quartet :: Painter -> Painter -> Painter -> Painter -> Painter
quartet p q r s = (p <-> q) </> (r <-> s)
