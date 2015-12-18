data BinaryTree = EmptyTree
    | Node Integer BinaryTree BinaryTree
    deriving (Show)

--Добавление элемента в дерево
insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree y = Node y EmptyTree EmptyTree

insert (Node c left right) y 
    | c == y = Node c left right
    | c < y = Node c left (insert right y)
    | c > y = Node c (insert left y) right

-- Удаление элемента
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree

remove (Node c EmptyTree EmptyTree) y
    | c == y = EmptyTree
    | otherwise = Node c EmptyTree EmptyTree

remove (Node c left EmptyTree) y
    | c < y = Node c left EmptyTree
    | c > y = Node c (remove left y) EmptyTree
    | c == y = left

remove (Node c EmptyTree right) y
    | c < y = Node c EmptyTree (remove right y)
    | c > y = Node c EmptyTree right
    | c == y = right

remove (Node c left right) y
    | c < y = Node c left (remove right y)
    | c > y = Node c (remove left y) right
    | c == y = left

-- Создание пустого дерева
emptyTree :: BinaryTree
emptyTree = EmptyTree

--Поиск элемента в дереве
containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False

containsElement (Node c left right) y 
    | c == y = True
    | c < y = containsElement right y
    | c > y = containsElement left y

-- Поиск в дереве наименьшего элемента, который больше или равен заданному
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = 0 -- Если дерево пустое, то возвращаем 0. Сигнализирует о том, что элемент не найден

nearestGE (Node x left right) y
    | x == y = y
    | x < y = nearestGE right y
    | x > y = findLessValue left y x
    where
        findLessValue :: BinaryTree -> Integer -> Integer -> Integer
        findLessValue (EmptyTree) y l = l
        findLessValue (Node x left right) y l
            | y == x = x
            | x < y = findLessValue right y l
            | x > y = findLessValue left y x

-- Создания дерева из списка
treeFromList :: [Integer] -> BinaryTree
treeFromList [] = EmptyTree

treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs)) (treeFromList (filter (>x) xs))

-- Создания списка из дерева
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []

listFromTree (Node x left right) = (listFromTree left) ++ x : (listFromTree right)

main = do
    print (emptyTree `insert` 1 `insert` 2 `insert` 3)
    print (remove (treeFromList [1,2,3]) 2)
    print (remove (treeFromList [1,2,3]) 4)
    print (remove (treeFromList [1,2,3]) 3)
    print (remove (treeFromList [1,2,3]) 1)
    print (insert (treeFromList [10,2,3]) 4)
    print (containsElement (treeFromList [10,2,3]) 5)
    print (containsElement (treeFromList [10,2,3]) 2)
    print (nearestGE (treeFromList [10,2,3]) 5)
    print (nearestGE (treeFromList [10,2,3]) 2)
    print (nearestGE (treeFromList [10,2,3]) 100)
    print (listFromTree (treeFromList [1,2,3]))