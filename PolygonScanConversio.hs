import CodeWorld
import Data.List
import Data.Text(pack, unpack)

gYMax = 10
gYMin = 0
gXMax = 10
gXMin = 0
 
type PointI = (Int, Int)
-- Int 坐标转换为 CodeWorld 接受的 Double
toPoint:: PointI -> Point
toPoint (x,y) = (fromIntegral x::Double, fromIntegral y::Double)

data EdgeI = EdgeI {
    x ::Double
    , detX ::Double
    , yMax ::Int
}

-- 由两端点转换为新边表节点
mkEdge:: Int -> Int -> Int -> Int -> EdgeI
mkEdge ax ay bx by 
  | ay > by = mkEdge bx by ax ay
  | otherwise = EdgeI (fromIntegral ax) detx by
                where detx = (fromIntegral bx - fromIntegral ax)
                            /(fromIntegral by - fromIntegral ay)

type NET = [(Int,[EdgeI])] -- 新边表
type AET = [EdgeI] -- 活动边表

main :: IO()
main = animationOf $ drawPolys [[(0,0), (2,3), (4,2),(0,0)]]

drawPolys:: [[PointI]] -> Double -> Picture
drawPolys polys seconds = 
                   coordinatePlane 
                   & drawAllPoly polys
                   & foldl (&) blank $ map drawDot 
                        (ploy_fill (gYMin-1) 
                                   (mkNewEdgeTables polys) -- NET
                                   []) -- AET
                   where 
                     lineCnt = 100 -- floor (seconds*0.5) + 1
                     
-- 建立新边表
mkNewEdgeTables:: [[PointI]] -> NET
mkNewEdgeTables [] = []
mkNewEdgeTables (x:xs) = mkNewEdgeTable x ++ mkNewEdgeTables xs

-- 建立某一个多边形的新边表
mkNewEdgeTable::[PointI] -> NET
mkNewEdgeTable [] = zip [gYMin .. gYMax] (repeat [])
mkNewEdgeTable [x] = zip [gYMin .. gYMax] (repeat []) -- 返回空表
mkNewEdgeTable (a@(ax, ay) : b@(bx,by) : xs) = if ay == by then mkNewEdgeTable (b:xs) -- 过滤水平边
                                               else insertE (min ay by) (mkEdge ax ay bx by) (mkNewEdgeTable (b:xs))  

-- 将边信息插入新边表
insertE :: Int -> EdgeI -> NET -> NET
insertE y edge (net@(posY,eds):nets) = if y == posY 
                                      then return (posY, (edge:eds)) ++ nets
                                      else return net ++ nets

-- 扫描线法主过程
ploy_fill:: Int -> NET -> AET -> [PointI]
ploy_fill y net aet = getYvalAet y aet 
                      ++ if y >= gYMax then [] else
                      ploy_fill (y+1)
                                   (filter (\(y',e) -> y'/= y+1) net) -- 删去过时的新边表
                                   ((if y == gYMin-1 then [] else (snd $ head net))  -- 从新边表中加入新的活动边
                                     ++ (filter (\aet@(EdgeI x dx yM) -> yM == y+1) aet)) -- 删去Ymax>y+1的活动边

-- 跟据y值和活性边表，求当前扫描线的所有交
getYvalAet:: Int -> AET -> [PointI]
getYvalAet y [] = []
getYvalAet y (aet@(EdgeI x dx _):aets) = (round x, y) : getYvalAet y aets

-- 画出所有多边形
drawAllPoly:: [[PointI]] -> Picture
drawAllPoly [] = blank
drawAllPoly (x:xs) = polygon (map toPoint x) & drawAllPoly xs  

-- 根据坐标画一个点
drawDot:: PointI -> Picture
drawDot (x, y) = translated (fromIntegral x) (fromIntegral y)
              $ solidCircle 0.3
