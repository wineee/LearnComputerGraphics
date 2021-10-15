{-
对于直线上的点, 那么 Ax + By + C = 0
对于直线上方的点, 那么 Ax + By + C > 0
对于直线下方的点, 那么 Ax + By + C < 0
将中点带入直线方程即可得知应取左上还是右下的点
过 (x0,y0), (x1,y1)
    A = y0 - y1;
	B = x1 - x0;
    C = y1x0-x1y0
let d(x,y) = Ax + By + C
we get:
    d0 = d(x0, y0+0.5) = 0.5B
    det0 = d(x+1, y) - d(x, y) = A 
    det1 = d(x+1, y+1) - d(x,y) = A+B 
为了避免小数，这里取2倍,不影响正负
    d0' = B
    det0' = 2A
    det1' = 2A+2B
-}
import CodeWorld
import Data.List
import Data.Text(pack, unpack)

main :: IO()
main = animationOf $ drawLinePoints (-5) (-5) 8  5

drawLinePoints:: Int -> Int -> Int -> Int -> Double -> Picture
drawLinePoints x0 y0 x1 y1 seconds = coordinatePlane 
                   & polyline([(fromIntegral x0, fromIntegral y0),
                               (fromIntegral x1, fromIntegral y1)])
                   & foldl1 (&) (take (round seconds+1) points)
                   where 
                     points = midPointDrawLine x0 y0 x1 y1

midPointDrawLine:: Int -> Int -> Int -> Int -> [Picture]
midPointDrawLine x0 y0 x1 y1 
  | x0 == x1 && y0 == y1 = [styledLettering Bold Handwriting 
                                  (pack "Not a Line")]
  | x0 > x1 = midPointDrawLine x1 y1 x0 y0                             
  | abs a <= abs b = zipWith drawDotWithMid' [x0 .. x1] 
                       -- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
                       $ unfoldr (\val@(x,y,d',isUp) -> if x <= x1 
                              then Just(
                                   (y, isUp),
                                   (x+1, 
                                   if d' > 0 then y else y+1,
                                   if d' > 0 then d'+det0 else d'+det1,
                                   d' > 0))
                              else Nothing) (x0, y0, d1, True)
  | otherwise = map (reflected (pi/4)) (midPointDrawLine y0 x0 y1 x1)
  where 
        a = y0 - y1 -- equal to -dety
        b = x1 - x0 -- equal to detx
        d1 = b + det0
        det0 = a + a
        det1 = a + a + b + b
        drawDotWithMid' x (y,isUp)= drawDotWithMid x y isUp

drawDotWithMid :: Int -> Int -> Bool -> Picture
drawDotWithMid x y isUp = translated (fromIntegral x) (fromIntegral y)
              $ colored green 
              $ solidCircle 0.3
              & translated 0 (if isUp then 0.5 else (-0.5)) midline
              where
                  midline = polygon [(-0.25,0),(0.25,0)]
