--problem1
myLast :: [a] -> a
myLast (va:[]) = va
myLast (va:vs) = myLast vs

--problem2
myButLast :: [a] -> a
myButLast (va:(vs:[]))  = va
myButLast (va:vs)       = myButLast vs

--problem3
elementAt :: [a] -> Int -> a
elementAt (vc:vs) 1   = vc
elementAt (vc:vs) n   = elementAt vs $ n-1
elementAt _ _         = error "Out of Bound"

--problem5 List Reversal
--Remember that you can use accumulation?

myReverse :: [a] -> [a]
myReverse list       = myReverse' list []
  where myReverse' :: [a] -> [a] -> [a]
        myReverse' [] acc       = acc
        myReverse' (vc:vs) acc  = myReverse' vs (vc:acc)


--problem6 Palindrome
--From this, I learned that you must enforce the Eq on variable a in order to have it evaluated for equality

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []       = True
isPalindrome (vc:[])  = True
isPalindrome (vc:vs)  = if vc == myLast vs 
                           then isPalindrome $ init vs
                           else False

--problem7
--Flattening a nested list structure
-- Data Constructors!!! Group them with parenthesis
--
data NestedList a = Elem a
                  | List [NestedList a]

flatten :: (NestedList a) -> [a]
flatten (Elem a)        = [a] 
flatten (List [])       = []
flatten (List (vc:vs))  = flatten vc ++ flatten (List vs)


--problem8
--Eliminate consecutive
--This problem might be solved like so:
--Get list, if next item is the same as the previous, concatenate original
--Base case could be like

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (vf:vs:vr) = if vf == vs
                         then vf : compress vr
                         else vf : compress (vs:vr)

--problem9 packing steel
--
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [a] = [[a]]
pack (vf:vr) = 
  let (list_a, list_b) = pack' [vf] vr
  in  [list_a] ++ pack list_b
  where pack' :: (Eq a) => [a] -> [a] -> ([a],[a])
        pack' vs []                = (vs,[])
        pack' (vf1:vr1) (vf2:vr2) = if vf1 == vf2 
                                       then pack' ((vf1 : vr1) ++ [vf2]) vr2 
                                       else ((vf1:vr1), (vf2:vr2))

--problem10 encode

encode :: (Eq a) => [a] -> [(Int, a)]
encode vs = 
  let list = pack vs
  in encode' list
  where encode' :: (Eq a) => [[a]] -> [(Int, a)]
        encode' (vf : [[]]) = [convertToTuple vf]
        encode' (vf : vr)   = convertToTuple vf : (encode' vr)
        encode' _           = []

        convertToTuple :: [a] -> (Int, a)
        convertToTuple vs = 
          let (vff:vrr) = vs
          in (length vs, vff)



data Encoding a = Single a
                | Multiple Int a
                deriving Show



--Problem11
--
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified vs = 
  let list = encode vs
  in  map (\(count, value) -> if count == 1 
                              then (Single value)
                              else (Multiple count value))
          list
 
--problem12

decodeModified :: [Encoding a] -> [a]
decodeModified vs = foldl (++) [] $ map decode' vs
  where decode' (Single value) = [value]
        decode' (Multiple count value) = take count (repeat value)

encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect vs = encode' vs 1
  where encode' (vf:vs:vr) count = if vf == vs
                                      then encode' (vf:vr) (count+1)
                                      else (createEncoding count vf) : encode' (vs:vr) 1
        encode' (vs:vr) count = [createEncoding count vs]
        createEncoding 1 value = Single value
        createEncoding a value = Multiple a value


dupli :: [a] -> [a]
dupli vs = repli vs 2

repli :: [a] -> Int -> [a]
repli (vs:[]) count = (take count (repeat vs))
repli (vs:vr) count = (take count (repeat vs)) ++ repli vr count

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt myChar list 1 = myChar:list
insertAt myChar (vf:vr) count = vf:insertAt myChar vr (count-1)

-- Problem 22
range :: Int -> Int -> [Int]
range lower upper = if lower == upper
                       then [lower]
                       else lower:range (lower+1) upper

-- To help some problems
indexAt :: [a] -> Int -> a
indexAt (vf:vr) 0 = vf
indexAt (vf:vr) n = indexAt vr (n-1)


-- Problem 26
--

combinations :: Int -> [a] -> [[a]]
combinations size list = combinations' size list [[]]
  where combinations' :: Int -> [a] -> [[a]] -> [[a]]
        combinations' _ list result = result


-- Problem 31
--


divides :: Int -> Int -> Bool
divides n1 n2 = n2 `mod` n1 == 0


isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = primeTest n 3
  where primeTest n testCase
          | testCase >= n = True
          | otherwise     = if testCase `divides` n
                               then False
                               else primeTest n $ (testCase+2)

-- Problem 32 - using Euclidean Algorithm

myGcd :: Int -> Int -> Int
myGcd n1 n2 
  | n1<n2     = myGcd' n2 n1 (n2 `mod` n1)
  | n2<n1     = myGcd' n1 n2 (n1 `mod` n2)
  | otherwise = n1
  where myGcd' n1 n2 0 = n2
        myGcd' n1 n2 m = myGcd' n2 m (n2 `mod` m)


coprime :: Int -> Int -> Bool
coprime n1 n2 = if gcd n1 n2 == 1
                   then True
                   else False


totient :: Int -> Int
totient n = totient' 1 0
  where totient' i acc
          | i > n     = acc
          | otherwise = totient' (succ i) ( if (coprime n i) 
                                               then (succ acc) 
                                               else acc )
