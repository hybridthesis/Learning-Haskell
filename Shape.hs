module Shape (Shape(Rectangle, Ellipse, RtTriangle, Polygon),
              Radius, Side, Vertex,
              area, triArea, distBetween,
              circle, square,
              )
  where

  data Shape = Rectangle Side Side
             | Ellipse Radius Radius 
             | RtTriangle Side Side
             | Polygon [Vertex]
    deriving Show

  type Radius = Float
  type Side = Float
  type Vertex = (Float, Float)

  circle :: Radius -> Shape
  circle r = Ellipse r r

  square :: Side -> Shape
  square s = Rectangle s s

  area :: Shape -> Float
  area (Rectangle s1 s2) = s1 * s2
  area (Ellipse r1 r2) = r1 * r2 * pi
  area (RtTriangle s1 s2) = s1 * s2 / 2
  area (Polygon (v1: vs)) = polyArea vs 
    where polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea (v3:vs')
          polyArea _ = 0

  distBetween :: Vertex -> Vertex -> Float
  distBetween (x1, y1) (x2, y2) = sqrt $ (y2 - y1)^2 + (x2 - x1)^2

  triArea :: Vertex -> Vertex -> Vertex -> Float
  triArea v1 v2 v3 = let a = distBetween v1 v2
                         b = distBetween v2 v3
                         c = distBetween v3 v1
                         s = (a + b + c) / 2
                     in sqrt $ s * (s - a) * (s - b) * (s - c)


