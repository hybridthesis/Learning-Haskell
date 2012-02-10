makeChange :: Int -> [Int] -> [Int]
makeChange 0 _  = []
makeChange amt (1:[]) = [ amt ]
makeChange amt (v1:vs) = [(amt `div` v1)] ++ makeChange (amt `mod` v1) vs


makeChange1 :: Int -> [Int] -> [Int]
makeChange1 amt vs = makeChange1' amt vs []
  where makeChange1' :: Int -> [Int] -> [Int] -> [Int]
        makeChange1' 0 _ acc = acc
        makeChange1' amt (v1:vs) acc = makeChange1' (amt `mod` v1) vs (acc ++ [(amt `div` v1)])

makeChange2 :: Int -> [Int] -> [Int]
makeChange2 amt vs = zipWith change vs $ swap $ map (foldl decAmt amt) $ grow vs
  where decAmt amt' x = amt' `mod` x
        difference x = amt - x
        swap vs = amt : init vs
        grow vs = [ take n vs | n <- [1..(length vs)]]
        change x y = y `div` x



