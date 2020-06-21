import Geometry 

areaCylinder = cylinderArea 2 10 -- 150.79645
volumeCylinder = cylinderVolume 2 10 -- 125.66371
volumeSphere = sphereVolume 2 -- 33.510323

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point | Triangle Point Point Point deriving (Show) 

getMag :: Point -> Point -> Float 
getMag (Point x1 y1) (Point x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2 

vertices :: Shape -> [Float]
vertices (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 
    let p1 = (Point x1 y1) 
        p2 = (Point x2 y2) 
        p3 = (Point x3 y3)
    in [getMag p1 p2, getMag p2 p3, getMag p1 p3]

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = 
    (abs $ x2 - x1) * (abs $ y2 - y1) 
surface (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 
    let s = (perimeter (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3))) / 2
        v = vertices (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3))
    in sqrt $ s * (s - v !! 0) * (s - v !! 1) * (s - v !! 2)

perimeter :: Shape -> Float 
perimeter (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =
    let v = vertices (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3))
    in (v !! 0) + (v !! 1) + (v !! 2)

baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height) 

myTriangle = Triangle (Point 0 0) (Point 0 1) (Point 1 1)