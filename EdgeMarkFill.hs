import CodeWorld
import Data.List
import Data.Text(pack, unpack)

main :: IO()
main = animationOf $ drawPolys [[(-1,-2), (2,3), (4,-5),(-1,-2)]]

drawPolys:: [[(Int, Int)]] -> Double -> Picture
drawPolys poly seconds = 
                   coordinatePlane 
                   & drawAllPoly poly
                   --polyline()
                   --(map drawDot (take ((floor (seconds*0.5)) + 1) points)))
                   --where 
                     --points = ddaDrawLine ax ay bx by

--edgemark_fill::
drawAllPoly:: [[(Int, Int)]] -> Picture
drawAllPoly [] = blank
drawAllPoly (x:xs) = foldl (&) blank (map drawDot $ drawPoly x) 
                     & polygon (map toPoint x)
                     & drawAllPoly xs
                     where
                     toPoint (x,y) = (fromIntegral x::Double, fromIntegral y::Double)
                   
                     
drawPoly:: [(Int, Int)] -> [(Int, Int)]
drawPoly [] = []
drawPoly [x] = []
drawPoly (a@(ax, ay) : b@(bx,by) : xs) = ddaDrawLine ax ay bx by ++ drawPoly (b:xs)  


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
