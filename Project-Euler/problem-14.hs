getEnder :: [(Int,Int)] -> Int -> Int
getEnder ((first, second):rest) ender = 
  if second == ender
     then getEnder rest $ max first ender
     else getEnder rest ender
getEnder _ ender = ender

filterCollatz :: [(Int,Int)] -> Int -> [(Int,Int)]
filterCollatz vs ender = filter f vs  
  where f (n1, n2) = 
          not ((n2 == ender) || (n2 == 1))

nextCollatz :: [(Int, Int)] -> [(Int, Int)]
nextCollatz vs = map f vs
  where f (n1, n2) =
          (n1, if even n2
                  then n2 `div` 2
                  else n2 * 3 + 1)

collatz :: [(Int, Int)] -> Int -> Int
collatz ((n1,n2):[]) _  = n1
collatz vs ender       = 
  collatz next newEnder
  where unfilteredNext = nextCollatz vs
        newEnder = getEnder unfilteredNext ender
        next = filterCollatz unfilteredNext newEnder


runCollatz = collatz [(x,x)| x <- [13..20]] 8
