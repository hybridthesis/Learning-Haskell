-- Test both up and down
-- Compare up and down to current position
-- If current position is better than both
-- Up and down
-- Then move left

tolerance = 0.7

getTriFromPerimeter :: Int -> (Int, Int)
getTriFromPerimeter perimeter = getTri' (perimeter `div` 2 - 1, perimeter `div` 2 - 1)
  where getTri' :: (Int, Int) -> (Int, Int) 
        getTri' coord = if verifyCoord coord
                           then coord
                           else getTri' $ improveCoord coord

        improveCoord :: (Int, Int) -> (Int, Int)
        improveCoord coord =
          let up    = guessUp coord
              down  = guessDown coord
              left  = guessLeft coord
              guessUp (x, y)    = (x, y+1)
              guessDown (x, y)  = (x, y-1)
              guessLeft (x, y)  = (x-1, y)
              guess = compareCoords coord (compareCoords down up)
          in  if guess == coord
                 then left
                 else guess

        compareCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)
        compareCoords coordA coordB = if (tryCoord coordA) <= (tryCoord coordB)
                                        then coordA
                                        else coordB

        tryCoord :: (Int, Int) -> Int
        tryCoord (x, y) = abs((perimeter - x - y)^2 - (x^2 + y^2))

        verifyCoord :: (Int, Int) -> Bool
        verifyCoord (x, y) = ((perimeter - x - y)^2 - (x^2 + y^2)) == 0
