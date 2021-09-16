import CodeWorld
import Data.List
import Data.Text(pack, unpack)

main :: IO()
main = animationOf $ drawLinePoints (-5) 7  5 (-7)

drawLinePoints:: Int -> Int -> Int -> Int -> Double -> Picture
drawLinePoints ax ay bx by seconds =  
                   polyline([(fromIntegral ax, fromIntegral ay),
                               (fromIntegral bx, fromIntegral by)])
                   & (foldl (&)  blank 
                   (map drawDot (take ((floor (seconds*0.5)) + 1) points)))
                   where 
                     points = ddaDrawLine ax ay bx by

--edgemark_fill::

drawPoly:: [(Int, Int)] -> [(Int, Int)]
drawPoly [] = []
drawPoly (a@(ax, ay) : b@(bx,by) : xs) = ddaDrawLine ax ay bx by ++ drawPoly xs  


ddaDrawLine:: Int -> Int -> Int -> Int -> [(Int, Int)]
ddaDrawLine ax ay bx by 
  | ax == bx && ay == by = []
  | ax > bx = ddaDrawLine bx by ax ay                             
  | dx >= (abs dy) = zip [ax .. bx] $ map round [fromIntegral ay, (fromIntegral ay) + k .. ]
  | otherwise = map swap (ddaDrawLine ay ax by bx)
  where dx = fromIntegral (bx - ax) ::Double
        dy = fromIntegral (by - ay) ::Double
        k = dy / dx
        swap (x, y) = (y, x)

drawDot :: (Int, Int) -> Picture
drawDot (x, y)= translated (fromIntegral x) (fromIntegral y)
              $ colored green
              $ solidCircle 0.3
