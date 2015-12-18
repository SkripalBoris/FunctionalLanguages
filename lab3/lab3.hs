-- ����� �������
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

-- ������ �������
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ a [] = a
foldr' f a (x:xs) =f x (foldr' f a xs)

-- ���������� �������� � ������ � ������� ������ �����������
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f list = foldr' ((:) . f) [] list

-- ���������� �������� � ������ ����� � ������� ������ �����������
flatMap' :: (t -> a) -> [t] -> [a]
flatMap' _ [] = []
flatMap' f list=  foldr'  buf [] list where buf a b = [f a] ++ b

-- ������
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f list = foldr' ((++) . buf) [] list where buf a | f a= [a]
                                                         | otherwise =[]

-- ������������ ���� �������
append' :: [a] -> [a] -> [a]
append' list [] = list
append' [] list = list
append' list1 list2 = foldr' (:) list2 list1

-- ��������� ������
reverse' :: [a] -> [a]
reverse' [] = []
reverse' list = foldr' f [] list where f x xs = xs ++ [x]

-- ��������� ������� ������
last' :: [a] -> a
last' [] = error "Empty list"
last' (x:xs) = foldl' f x xs where f _ x = x

-- ���������� �������� � ����� ������
snoc' :: [a] -> a -> [a]
snoc' [] _ = []
snoc' xs a= foldr' (:) [a] xs

list=[1,2,3]
list2=[5,6,7]
main= do
    print(show(foldl' (+) 0 list))
    print(show(foldr' (+) 0 list))
    print(show(map' (+1) list))
    print(show(flatMap' (*2) list))
    print(show(filter' (>6) list))
    print(show(filter' (>6) list2))
    print(show(append' list list2))
    print(show(reverse' list))
    print(show(last' list))
    print(show(snoc' list 4))