data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1, tree2

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) = case compare x y of
                                         LT => Node (insert x left) y right
                                         EQ => orig
                                         GT => Node left y (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

|||naive and expensive.  Can we do better? (constraint: In order traversal)
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node tree x tree1) = treeToList tree ++ [x] ++ treeToList tree1
