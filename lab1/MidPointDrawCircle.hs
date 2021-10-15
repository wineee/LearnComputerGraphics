{-
设圆心为（0，0）
f(x,y) = x^2 + y^2 - r^2
若 <0 中点在圆的内侧，取圆外点
...

由于圆的对称，取 45-90度 的圆弧

中点画圆法
判断 d = f(x+1,y-0.5) 与 0 大小

d0 = f(1, r-0.5) = 1.25-r

d < 0 取外部点 {
   x' = x+1
   y' = y
   d' = f(x+2, y-0.5)
   det0 = d' - d = 2*x+3
} else {
   x' = x+1
   y' = y-1
   d' = f(x+2, y-1.5)
   det1 = d' - d = 2*x-2*y+5
}

-}
{-# LANGUAGE BlockArguments #-}

import CodeWorld
import Data.List
import Data.Text (pack, unpack)

main :: IO ()
main = animationOf $ drawLineCircle 7

drawLineCircle :: Int -> Double -> Picture
drawLineCircle r seconds =
  coordinatePlane
    & circle (fromIntegral r)
    & selfAndMirror (pi/2) black 
      . selfAndMirror 0 blue 
      . selfAndMirror (pi/4) red  
      $ foldl1 (&) (take (round seconds + 1) points)
  where
    points = midPointDrawCircle r
    selfAndMirror angle color pic  = pic & (colored color (reflected angle pic))


midPointDrawCircle :: Int -> [Picture]
midPointDrawCircle r
  | r <= 0 = [ styledLettering Bold Handwriting
                         (pack "Not a Circle")]
  | otherwise = map (\(x, y, _) -> drawDot (fromIntegral x ::Int) 
                                           (fromIntegral y ::Int))
                $ takeWhile (\(x, y, _) -> (x <= y))
                $ iterate (\(x, y, d') ->
                            let det0 = x + x + 3
                                det1 = x + x - y - y + 5
                            in ( x + 1,
                                if d' < 0 then y else y - 1,
                                if d' < 0 then d' + det0 else d' + det1
                            ))
                (0, r, 1 - r) -- 1.25-r 除1.25外，所有操作为整数，所以0.25可以去掉

drawDot :: Int -> Int -> Picture
drawDot x y =
  translated (fromIntegral x) (fromIntegral y) $
    colored green $
      solidCircle 0.3
