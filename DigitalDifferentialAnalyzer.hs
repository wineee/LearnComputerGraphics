import CodeWorld
import Data.List
import Data.Text(pack, unpack)

main :: IO()
main = animationOf $ getLinePoints (-5) 7  5 (-7)

getLinePoints:: Int -> Int -> Int -> Int -> Double -> Picture
getLinePoints ax ay bx by seconds = coordinatePlane 
                   & polyline([(fromIntegral ax, fromIntegral ay),
                               (fromIntegral bx, fromIntegral by)])
                   & foldl1 (&) (take (round seconds+1) points)
                   where 
                     points = ddaDrawLine ax ay bx by

ddaDrawLine:: Int -> Int -> Int -> Int -> [Picture]
ddaDrawLine ax ay bx by 
  | ax == bx && ay == by = [styledLettering Bold Handwriting 
                                  (pack "Not a Line")]
  | ax > bx = ddaDrawLine bx by ax ay                             
  | dx >= (abs dy) = zipWith drawDot [ax .. bx] 
               $ map round [fromIntegral ay, (fromIntegral ay) + k .. ]
  | otherwise = map (reflected (pi/4)) (ddaDrawLine ay ax by bx)
  where dx = fromIntegral (bx - ax) ::Double
        dy = fromIntegral (by - ay) ::Double
        k = dy / dx

drawDot :: Int -> Int -> Picture
drawDot x y = translated (fromIntegral x) (fromIntegral y)
              $ colored green
              $ solidCircle 0.3
