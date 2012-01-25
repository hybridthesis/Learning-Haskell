module Main where
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

