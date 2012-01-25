module Perimeter (perimeter,
                  module Shape
                  ) where
  import Shape


  perimeter :: Shape -> Float
  perimeter (Rectangle s1 s2) = 2 + s1 * s2 
  perimeter (RtTriangle s1 s2) = s1 + s2 + sqrt (s1^2 + s2^2)
  perimeter (Polygon vs) = foldl (+) 0 $ sides vs

  sides :: [Vertex] -> [Side]
  sides [] = []
  sides (v:vs) = aux v vs
    where aux v1 (v2:vs') = distBetween v1 v2:aux v2 vs'
          aux vn [] = distBetween vn v : []
