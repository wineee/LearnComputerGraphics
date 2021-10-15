{-
判断直线 y = kx+b 与 x=x1 的交点靠近上方还是下方
设 dy 为交点与下面像素点距离

dy <> 0.5
dy - 0.5 <> 0
其中 e = -0.5, dy 可以累计， x加1，dy加k=dy/dx

----- 消除浮点运算,同乘 2*dx ------
dy*2*dx <> 0.5*2*dx   
即判断
dy*2*dx - 0.5*2*dx <> 0
dy*2*dx - dx <> 0
其中 e' = -dx, dy'=dy*2*dx 可以累计， x加1，dy'加k' = dy*2

-}
import CodeWorld
import Data.List
import Data.Text(pack, unpack)

main :: IO()
main = animationOf $ drawLinePoints (-5) (-5) 8  14

drawLinePoints:: Int -> Int -> Int -> Int -> Double -> Picture
drawLinePoints x0 y0 x1 y1 seconds = coordinatePlane 
                   & polyline([(fromIntegral x0, fromIntegral y0),
                               (fromIntegral x1, fromIntegral y1)])
                   & foldl1 (&) (take (round seconds+1) points)
                   where 
                     points = bresenhamDrawLine x0 y0 x1 y1

bresenhamDrawLine:: Int -> Int -> Int -> Int -> [Picture]
bresenhamDrawLine x0 y0 x1 y1 
  | x0 == x1 && y0 == y1 = [styledLettering Bold Handwriting 
                                  (pack "Not a Line")]
  | x0 > x1 = bresenhamDrawLine x1 y1 x0 y0                             
  | dx >= abs dy = zipWith drawDot [x0 .. x1] 
                       -- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
                       $ unfoldr (\val@(x,y,ne) -> if x <= x1 
                              then Just(
                                   y,
                                   (x+1, 
                                   if ne > 0 then y+1 else y,
                                   if ne > 0 then ne-one'+k' else ne+k'))
                              else Nothing) (x0, y0, e'+k') -- (-0.5)+k)
  | otherwise = map (reflected (pi/4)) (bresenhamDrawLine y0 x0 y1 x1)
  where 
        dx = fromIntegral (x1 - x0) ::Double
        dy = fromIntegral (y1 - y0) ::Double
        e = 0.5
        k = dy / dx
        one = 1
        ------------
        e' = -dx
        k' = dy + dy
        one' = dx + dx

drawDot :: Int -> Int -> Picture
drawDot x y = translated (fromIntegral x) (fromIntegral y)
              $ colored green 
              $ solidCircle 0.3
