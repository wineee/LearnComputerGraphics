import CodeWorld
import Data.List
import Data.Text(pack, unpack)

main :: IO()
main = animationOf $ drawPolys [[(-1,-2), (2,3), (4,-5),(-1,-2)]]

drawPolys:: [[(Int, Int)]] -> Double -> Picture
drawPolys polys seconds = 
                   coordinatePlane 
                   & drawAllPoly polys
                   & foldl1 (&) (take lineCnt $ edgemark_fill (-6) 9 (-9) 9 (getAllPolyP polys))
                   where 
                     lineCnt = 100 -- floor (seconds*0.5) + 1

edgemark_fill:: Int -> Int -> Int -> Int -> [(Int, Int)] -> [Picture]
edgemark_fill xMin xMax yMin yMax edgeP = [scanLine True xMin y | y <- [yMin .. yMax]]
                     where scanLine:: Bool -> Int -> Int -> Picture
                           scanLine st x y = if x == xMax then blank else
                                             if odd (count (x,y) edgeP)
                                             then scanLine (not st) (x+1) y
                                             else scanLine st (x+1) y
                                               & (if st then colored red $ drawDot (x,y) else blank)
                           count a as= sum [1 | b <- as, b == a]

drawAllPoly:: [[(Int, Int)]] -> Picture
drawAllPoly [] = blank
drawAllPoly (x:xs) = polygon (map toPoint x)
                     & drawAllPoly xs
                     where
                     toPoint (x,y) = (fromIntegral x::Double, fromIntegral y::Double)

getAllPolyP:: [[(Int, Int)]] -> [(Int, Int)]
getAllPolyP [] = []
getAllPolyP (x:xs) = getPolyP x ++ getAllPolyP xs  
                     
getPolyP:: [(Int, Int)] -> [(Int, Int)]
getPolyP [] = []
getPolyP [x] = []
getPolyP (a@(ax, ay) : b@(bx,by) : xs) = ddaGetLineP ax ay bx by ++ getPolyP (b:xs)  


ddaGetLineP:: Int -> Int -> Int -> Int -> [(Int, Int)]
ddaGetLineP ax ay bx by 
  | ax == bx && ay == by = []
  | ax > bx = ddaGetLineP bx by ax ay                             
  | dx >= (abs dy) = zip [ax .. bx] $ map round [fromIntegral ay, (fromIntegral ay) + k .. ]
  | otherwise = map swap (ddaGetLineP ay ax by bx)
  where dx = fromIntegral (bx - ax) ::Double
        dy = fromIntegral (by - ay) ::Double
        k = dy / dx
        swap (x, y) = (y, x)

drawDot :: (Int, Int) -> Picture
drawDot (x, y) = translated (fromIntegral x) (fromIntegral y)
              $ solidCircle 0.3
