module Shape (Shape(Rectangle, Ellipse, RtTriangle, Polygon),
              Radius, Side, Vertex,
              area, triArea, distBetween,
              circle, square,
              examplePolygon,
              exampleConcavePolygon 
              )
  where

  data Shape = Rectangle Side Side
             | Ellipse Radius Radius 
             | RtTriangle Side Side
             | Polygon [Vertex]
    deriving Show

  {-
    - The Shapes are defined intuitively,
    - However, the Polygon must be convex to make sense
    - Also, you should implement the concave Polygon aka general Polygon
    - To create polygon, you add up all the vertices from v0 to vn for a n-1 sided polygon
    - Remember: counter clockwise
    -}

  examplePolygon = Polygon [(0,0),(4,0), (5,5), (0,5)]

  exampleConcavePolygon = Polygon [(0,0),(4,0), (5,5), (0,5), (1,3)]

  -- Used with zipping two lists
  swap (va:vs) = vs ++ [va]

  type Radius = Float
  type Side = Float
  type Vertex = (Float, Float)
  type Ray = (Float, Float)
  
  circle :: Radius -> Shape
  circle r = Ellipse r r

  square :: Side -> Shape
  square s = Rectangle s s

  area :: Shape -> Float
  area (Rectangle s1 s2) = s1 * s2
  area (Ellipse r1 r2) = r1 * r2 * pi
  area (RtTriangle s1 s2) = s1 * s2 / 2
  -- This is the non-higher order function, recursive algorithm
  --
  -- area (Polygon (v1: vs)) = polyArea vs 
  --   where polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea (v3:vs')
  --         polyArea _ = 0
  --
  -- This IS the higher order function
  area (Polygon (v1:vs)) =
    let vertices          :: [(Vertex, Vertex)]
        vertices          = init $ zip vs $ swap vs
        triArea' (v2, v3) = triArea v1 v2 v3
        areas             = map triArea' vertices
    in  sum areas
        

  distBetween :: Vertex -> Vertex -> Float
  distBetween (x1, y1) (x2, y2) = sqrt $ (y2 - y1)^2 + (x2 - x1)^2

  triArea :: Vertex -> Vertex -> Vertex -> Float
  triArea v1 v2 v3 = let a = distBetween v1 v2
                         b = distBetween v2 v3
                         c = distBetween v3 v1
                         s = (a + b + c) / 2
                     in sqrt $ s * (s - a) * (s - b) * (s - c)


  isConvex :: Shape -> Bool
  isConvex (Polygon vs) = 
    let rays = zipWith fn vs $ swap vs

        -- Zips the vertices together to form a list of rays
        fn :: Vertex -> Vertex -> Ray
        fn (a1, a2) (b1, b2) = 
          (b1 - a1, b2 - a2)

        -- Takes two consecutive rays and determine if they are counter close wise convex or not
        isConvex' :: Ray -> Ray -> Bool
        isConvex' (a1, a2) (b1, b2) = 
          (a2 * b1) <= (a1 * b2)

    in  foldr (&&) True $ zipWith isConvex' rays $ swap rays

  isConvex _            = 
    True
