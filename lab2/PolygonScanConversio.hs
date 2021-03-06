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
type FIXD = [PointI]

main :: IO()
main = animationOf $ drawPolys [[(0,0), (2,8), (6, 2), (0,0)]]
 
drawPolys:: [[PointI]] -> Double -> Picture
drawPolys polys seconds = 
                   coordinatePlane 
                   & drawAllPoly polys
                   & (foldl (&) blank $ map drawDot 
                        (concat $ take lineCnt $ 
                         ploy_fill (gYMin-1) 
                                   (mkNewEdgeTables polys) -- NET
                                   []
                                   (mkFixDots polys)))-- AET
                   -- & colored red $ drawDot ((length (mkFixDots polys)),0)
                   where 
                     lineCnt = floor (seconds*1.0) + 1
                     
-- 建立新边表
mkNewEdgeTables:: [[PointI]] -> NET
mkNewEdgeTables [] = []
mkNewEdgeTables (x:xs) = mkNewEdgeTable x ++ mkNewEdgeTables xs

-- 建立某一个多边形的新边表
mkNewEdgeTable:: [PointI] -> NET
mkNewEdgeTable [] = zip [gYMin .. gYMax] (repeat [])  -- 返回空表
mkNewEdgeTable [x] = mkNewEdgeTable []
mkNewEdgeTable (a@(ax, ay) : b@(bx,by) : xs) = if ay == by then mkNewEdgeTable (b:xs) -- 过滤水平边
                                               else insertE (min ay by) (mkEdge ax ay bx by) (mkNewEdgeTable (b:xs))
mkFixDots:: [[PointI]] -> FIXD
mkFixDots [] = []
mkFixDots (x:xs) = mkFixDot ((last $ init x):x) $ mkFixDots xs

mkFixDot:: [PointI] -> FIXD -> FIXD
mkFixDot [] fd = fd
mkFixDot [x] fd = fd
mkFixDot (a:b:[]) fd = fd
mkFixDot (a@(ax, ay) : b@(bx,by) : c@(cx,cy) : xs) fd = if ay==by || by==cy || (ay<by) == (cy<by)
                                                        then mkFixDot (b:c:xs) fd
                                                        else b : mkFixDot (b:c:xs) fd


-- 将边信息插入新边表
insertE :: Int -> EdgeI -> NET -> NET
insertE y edge (net@(posY,eds):nets) = if y == posY 
                                       then return (posY, (edge:eds)) ++ nets
                                       else return net ++ insertE y edge nets

-- 扫描线法主过程
ploy_fill:: Int -> NET -> AET -> FIXD -> [[PointI]]
ploy_fill y net aet fd = zip (dotToLine $ sort $ map fst $ getYvalAet y aet ++ filter (\(_,y')->y'==y) fd) (repeat y) 
                      : if y == gYMax then [] else
                      ploy_fill (y+1)
                                (tail net) --(filter (\(y',e) -> y'/= y+1) net) -- 删去过时的新边表
                                ((snd $ head net)  -- 从新边表中加入新的活动边
                                 ++ getNext y aet)
                                fd 
                      where getNext:: Int -> AET -> AET
                            getNext y [] = []
                            getNext y (aet@(EdgeI x dx yM):aets)
                              | y == yM = getNext y aets -- 删去Ymax>y+1的活动边
                              | otherwise = aet { x = x+dx } : getNext y aets -- 更新 x 坐标

dotToLine:: [Int] -> [Int]
dotToLine dots = dotToLine' ([gXMax-1] ++ dots ++ [gXMax+1]) False

dotToLine':: [Int] -> Bool -> [Int]
dotToLine' [] st = []
dotToLine' [x] st = []
dotToLine' (a:b:xs) st = (if st then [a .. b] else []) ++ dotToLine' (b:xs) (not st)

-- 跟据y值和活性边表，求当前扫描线的所有交
getYvalAet:: Int -> AET -> [PointI]
getYvalAet y [] = []
getYvalAet y (aet@(EdgeI x _ _):aets) = (round x, y) : getYvalAet y aets

-- 画出所有多边形
drawAllPoly:: [[PointI]] -> Picture
drawAllPoly [] = blank
drawAllPoly (x:xs) = polygon (map toPoint x) & drawAllPoly xs  

-- 根据坐标画一个点
drawDot:: PointI -> Picture
drawDot (x, y) = translated (fromIntegral x) (fromIntegral y)
              $ solidCircle 0.3
