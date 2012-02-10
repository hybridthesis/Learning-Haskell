module Tree (
            Tree(Empty, Branch),
            leaf,
            )
  where

  data Tree a = Empty 
              | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

  t_a = Branch 'x' (leaf 'x') (Branch 'x' (leaf 'x') (leaf 'x'))
  t_b = Branch 'x' (Branch 'x' (leaf 'x') (leaf 'x')) (leaf 'x') 

  leaf x = Branch x Empty Empty

  treeElt (Branch a t1 t2) = a

  treeLeft (Branch a t1 t2) = t1
  treeRight (Branch a t1 t2) = t2

  removeDuplicate (v1:[]) = [v1]
  removeDuplicate (v1:vs) = if (foldl (\x y-> x || (y==v1)) False vs)
                               then removeDuplicate vs
                               else v1:removeDuplicate vs

  {-
    - While n is not zero
    - Keep trying
    -}
  -- cbalTree :: Int -> [(Tree Char)]
  -- cbalTree n = cbalTree' n [(leaf 'x')] 
  -- -- 
  -- cbalTree' :: Int -> [Tree a] -> [Tree a]
  -- cbalTree' 0 acc = acc
  -- cbalTree' n acc = cbalTree' (n-1) $ acc ++ (foldr (++) [] $ map treePermute acc)


  treeIsLeaf :: (Tree a) -> Bool
  treeIsLeaf (Branch x Empty Empty) = True
  treeIsLeaf _ = False

  treeSize :: (Tree a) -> Int
  treeSize (Branch a Empty Empty)   = 1
  treeSize (Branch a t1 t2)         = 1 + (treeSize t1) + (treeSize t2)
  treeSize _ = 0

  -- Returns the amount of empty spaces in a tree
  emptyCount = (+1) . treeSize



  -- Returns the positions of the empty locations
  -- For example a tree like so
  -- 
  --            'x'
  --           /   \
  --         'x'  'x'
  --             /   \
  --           'x'  'x'
  --
  -- Would return [0,1,2,3,4,5]
  -- Which are the empty positions
  treeEmptyArray t = enumFromTo 0 $ (emptyCount t) - 1

  -- Takes in a tree and a 'n'
  -- Replace the nth empt with a leaf 'x'
  treeReplaceEmpty :: (Tree Char) -> Int -> (Tree Char)
  treeReplaceEmpty Empty 0 = leaf 'x'
  treeReplaceEmpty (Branch a t1 t2) n = 
    Branch a (treeReplaceEmpty t1 n) $ treeReplaceEmpty t2 (n - (emptyCount t1))
  treeReplaceEmpty tree _ = tree

  treeAddOne      :: (Tree Char) -> [Tree Char]
  treeAddOne Empty = [leaf 'x']
  treeAddOne tree = 
    removeDuplicate $ filter treeIsBalanced $ map (treeReplaceEmpty tree) (treeEmptyArray tree)


  treeIsBalanced (Branch _ t1 t2) = if abs ((treeSize t1) - (treeSize t2)) > 1
                                       then False
                                       else (treeIsBalanced t1) && (treeIsBalanced t2)
  treeIsBalanced Empty = True

  {- symmetric :: Tree a -> Bool -}

  treePermutate :: [Tree Char] -> [Tree Char]
  treePermutate trees = foldl (++) [] $ map treeAddOne trees

  cbalTree    :: Int -> [Tree Char]
  cbalTree n = removeDuplicate $ cbal' [Empty]
    where cbal' = foldr (.) id $ take n $ repeat treePermutate
