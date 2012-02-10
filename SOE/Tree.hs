data Tree a = Leaf a | Branch (Tree a) (Tree a)
            deriving Show


{-
  - Generic function for tree operation
  -}

permeate :: (Tree a) -> (b -> b -> b) -> (a -> b) -> b
permeate (Leaf x) fn1 fn2  = fn2 x
permeate (Branch t1 t2) fn1 fn2  = (permeate t1 fn1 fn2) `fn1` (permeate t2 fn1 fn2)


fringe :: (Tree a) -> [a]
fringe t = permeate t (++) (:[])

treeSize :: (Num b) => (Tree a) -> b
treeSize t = permeate t (+) (\x->1)


-- So you may use the data constructors as the functions and they can curry in params
--

mapTree f t = permeate t Branch (\x -> (Leaf (f x)))
