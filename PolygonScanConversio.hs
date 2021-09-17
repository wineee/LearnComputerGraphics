import CodeWorld
import Data.List
import Data.Text(pack, unpack)

gYMax = 10
gYMin = 0
gXMax = 10
gXMin = 0
 
type PointI = (Int, Int)
toPoint:: PointI -> Point
toPoint (x,y) = (fromIntegral x::Double, fromIntegral y::Double)

data EdgeI = EdgeI {
    x ::Int
    , detX ::Double
    , yMax ::Int
}

mkEdge:: Int -> Int -> Int -> Int -> EdgeI
mkEdge ax ay bx by 
  | ay > by = mkEdge bx by ax ay
  | otherwise = EdgeI ax detx by
                where detx = (fromIntegral bx - fromIntegral ax)
                            /(fromIntegral by - fromIntegral ay)

type NET = [(Int,[EdgeI])] -- 新边表
type AET = NET -- 活动边表

main :: IO()
main = animationOf $ drawPolys [[(-1,-2), (2,3), (4,-5),(-1,-2)]]

drawPolys:: [[PointI]] -> Double -> Picture
drawPolys polys seconds = 
                   coordinatePlane 
                   & drawAllPoly polys
                   -- & foldl1 (&) (take lineCnt $ edgemark_fill (-6) 9 (-9) 9 (getAllPolyP polys))
                   
                   where 
                     lineCnt = 100 -- floor (seconds*0.5) + 1
                     

mkNewEdgeTables:: [[PointI]] -> NET
mkNewEdgeTables [] = []
mkNewEdgeTables (x:xs) = mkNewEdgeTable x ++ mkNewEdgeTables xs

mkNewEdgeTable::[PointI] -> NET
mkNewEdgeTable [] = zip [0..10] (repeat [])
mkNewEdgeTable [x] = zip [0..10] (repeat [])
mkNewEdgeTable (a@(ax, ay) : b@(bx,by) : xs) = if ay == by then mkNewEdgeTable (b:xs)
                                               else insertE (min ay by) (mkEdge ax ay bx by) (mkNewEdgeTable (b:xs))  

insertE :: Int -> EdgeI -> NET -> NET
insertE y edge (net@(posY,eds):nets) = if y == posY 
                                      then return (posY, (edge:eds)) ++ nets
                                      else return net ++ nets


ploy_fill:: Int -> NET -> AET -> [PointI]
ploy_fill y net aet = if y > gYMax then [] else
                      getYvalAet y aet 
                      ++ ploy_fill (y+1)
                                   (filter (\(y',e) -> y'/= y+1) net)
                                   (filter (\(y',e) -> y'== y+1) net ++ aet)

-- 跟据y值和活性边表，求当前扫描线的所有交
getYvalAet:: Int -> AET -> [PointI]
getYvalAet y [] = []
getYvalAet y (aet@(sy, edges):aets) = map (\edge@(EdgeI x dx _) -> (x+round(dx*(fromIntegral y - fromIntegral sy)), y)) edges
                                      ++ getYvalAet y aets
-- type NET = [(Int,[EdgeI])] -- 新边表

drawAllPoly:: [[PointI]] -> Picture
drawAllPoly [] = blank
drawAllPoly (x:xs) = polygon (map toPoint x) & drawAllPoly xs

getAllPolyP:: [[PointI]] -> [PointI]
getAllPolyP [] = []
getAllPolyP (x:xs) = getPolyP x ++ getAllPolyP xs  
                     
getPolyP:: [PointI] -> [PointI]
getPolyP [] = []
getPolyP [x] =[]
getPolyP (a@(ax, ay) : b@(bx,by) : xs) =  ddaGetLineP ax ay bx by ++ getPolyP (b:xs)  

ddaGetLineP:: Int -> Int -> Int -> Int -> [PointI]
ddaGetLineP ax ay bx by 
  | ax == bx && ay == by = []
  | ax > bx = ddaGetLineP bx by ax ay                             
  | dx >= (abs dy) = zip [ax .. bx] $ map round [fromIntegral ay, (fromIntegral ay) + k .. ]
  | otherwise = map swap (ddaGetLineP ay ax by bx)
  where dx = fromIntegral (bx - ax) ::Double
        dy = fromIntegral (by - ay) ::Double
        k = dy / dx
        swap (x, y) = (y, x)

drawDot :: PointI -> Picture
drawDot (x, y) = translated (fromIntegral x) (fromIntegral y)
              $ solidCircle 0.3
