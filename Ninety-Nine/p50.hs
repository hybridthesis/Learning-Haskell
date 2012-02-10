import Tree

tree1 = leaf 'x'

tree2 = Empty

tree3 = Branch 'x' (Branch 'x' Empty Empty)
                   (Branch 'x' Empty Empty)

tree4 = Branch 'x' (leaf 'x')
                   (Branch 'x' Empty
                               (leaf 'x'))

tree5 = Branch 'x' (Branch 'x' (leaf 'x')
                               (leaf 'x'))
                   (Branch 'x' Empty
                               (leaf 'x'))

tree6 = Branch 'x' (Branch 'x' (leaf 'x') Empty)
                   (Branch 'x' Empty (leaf 'x'))



{-
  - cbalTree
In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
-}

{- prob55 = cbalTree 4 -}


{-
  - symmetric
Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
-}

