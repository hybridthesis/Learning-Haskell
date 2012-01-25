module Main where
import SOE


spaceClose :: Window -> IO ()
spaceClose w
            = do k <- getKey w
                 if k ==' ' then closeWindow w
                            else spaceClose w


--Sierpinski's Triangle
-- -- {{

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size
  = drawInWindow w (withColor Blue
      (polygon [(x, y), (x + size, y), (x, y - size), (x, y)]))


sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size
              = if size <= minSize
                then fillTri w x y size
                else let size2 = size `div` 2
                  in do sierpinskiTri w x y size2
                        sierpinskiTri w x (y - size2) size2
                        sierpinskiTri w (x + size2) y size2

main1
  = runGraphics (
    do w <- openWindow "S Tri" (400, 450)
       sierpinskiTri w 50 400 300
       spaceClose w
       )
-- }}

-- Snowflake fractal
-- {{


minSize :: Int
minSize = 8

mulcos30 :: Int -> Int
mulcos30 n = n * 86603 `div` 100000

fillSnowFlake :: Window -> Int -> Int -> Int -> IO ()
fillSnowFlake w x y size
-- First you draw the upside down equilateral
  = do drawInWindow w (withColor Blue
        (polygon [(x + size `div` 2, y), --Bottom 
                  (x, y - (mulcos30 size)),  --Upper left
                  (x + size, y - (mulcos30 size)),  --Upper right
                  (x + size `div` 2, y)]))
--Now draw the rightside up version
       drawInWindow w (withColor Blue
        (polygon [(x, y - (mulcos30 (size `div` 3))), --Bottom left
                  (x + size `div` 2, y - 4 * (mulcos30 (size `div` 3))), --Tip
                  (x + size, y - (mulcos30 (size `div` 3))), --Bottom Right
                  (x, y - (mulcos30 (size `div` 3))) ] ))

snowFlakeFractal :: Window -> Int -> Int -> Int -> IO ()
snowFlakeFractal w x y size
  = do if size <= minSize
       then fillSnowFlake w x y size
       else let size2     = size `div` 3
                y_offset  = (mulcos30 size `div` 9)
                x_center  = x + size2
                x_left    = x
                x_right   = x + 2 * size2
                y_top     = y - 4 * (mulcos30 size2) + 4 * y_offset
                y_second  = y - (mulcos30 size) + 3 * y_offset
                y_third   = y - (mulcos30 size2) + y_offset
                y_bottom  = y
         in do fillSnowFlake w x y size
               snowFlakeFractal w x_center y_top size2 --Top
               snowFlakeFractal w x_left y_second size2 --TopLeft
               snowFlakeFractal w x_right y_second size2 --TopRight
               snowFlakeFractal w x_left y_third size2 --BottomLeft
               snowFlakeFractal w x_right y_third size2 --BottomRight
               snowFlakeFractal w x_center y_bottom size2 --Bottom

main0
  = runGraphics (
    do w <- openWindow "S Tri" (800, 900)
       snowFlakeFractal w 100 800 600
       spaceClose w
       )
