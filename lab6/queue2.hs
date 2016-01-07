--Метод банкира
--Представим, что за добавление элемента в начало или в конец очереди мы зараба-
--тываем 3$. Каждая примитивная операция стоит 1$. Следовательно, при добавлении
--нового элемента в очередь мы сразу же тратим 1$ на то, что бы его добавить. Из
--этого следует, что при добавлении N элементов мы зарабатываем 2N$. Рассмотрим
--случай, когда при добавлении элемента в начало, размер "стека-начала" на 2
--больше, чем размер "стека-конца". В этом случае мы тратим 1$ на удаление эле-
--мента из первого стека и 1$ на добавление его во второй стек. Тем самым, мы
--послность тратим все 3$, которые заработали на добавлении элемента, и, следо-
--вательно, мы вышли в ноль. Тем самым, данный алгоритм имеет константную слож-
--ность.
--------------------------------------------------------------------------------
--Метод физика
--Считаем, что потенциал вектора равен N+M, где N - количество элементов в
--первом стеке, а M - количество элементов во втором стеке.
--Добавление в начало или в конец очереди без балансировка расходует О(1) и
--увеличивает потециал на 1, О(1)+1=О(1)
--Удаление из начала или конеца очереди без балансировки расходует О(1) и
--уменьшает потециал на 1, О(1)-1=О(1)
--Балансировка очереди не изменяет потенциал и расходует O(M+N)
--Следовательно, можно сделать вывод, что все операции добавления/удаления имеют
--константную сложность
data Queue a = Empty
             | Queue [a] [a] Int Int
             deriving (Show)

-- Сортировка очереди
createQueue :: [a] -> [a] -> Int -> Int -> (Queue a)
createQueue inStack outStack inSize outSize
    | inSize - outSize == 2 = Queue (take (inSize - 1) inStack) (outStack ++ [(last inStack)]) (inSize - 1) (outSize + 1)
    | outSize - inSize == 2 = Queue (inStack ++ [(last outStack)]) (take (outSize - 1) outStack) (inSize + 1) (outSize - 1)
    | otherwise             = Queue inStack outStack inSize outSize

--Добавление в конец
enqueueBack :: Queue a -> a -> Queue a
enqueueBack Empty x = Queue [] [x] 0 0
enqueueBack (Queue inStack outStack inSize outSize) x = createQueue inStack (x:outStack) inSize (outSize + 1)

--Добавление в начало 
enqueueTop :: Queue a -> a -> Queue a
enqueueTop Empty x = Queue [x] [] 1 0
enqueueTop (Queue inStack outStack inSize outSize) x = createQueue (x:inStack) outStack (inSize + 1) outSize

--Удаление из начала
dequeueTop :: Queue a -> (Queue a, a)
dequeueTop Empty = error ""
dequeueTop (Queue [] [] _ _) = error ""
dequeueTop (Queue [] [x] 0 1) = (Queue [] [] 0 0, x)
dequeueTop (Queue [x] [] 1 0) = (Queue [] [] 0 0, x)
dequeueTop (Queue (inHead:inTail) outStack inSize outSize) = (createQueue inTail outStack (inSize - 1) outSize, inHead)

--Удаление из конца 
dequeueBack :: Queue a -> (Queue a, a)
dequeueBack Empty = error ""
dequeueBack (Queue [] [] _ _) = error ""
dequeueBack (Queue [x] [] 1 0) = (Queue [] [] 0 0, x)
dequeueBack (Queue [] [x] 0 1) = (Queue [] [] 0 0, x)
dequeueBack (Queue inStack (outHead:outTail) inSize outSize) = (createQueue inStack outTail inSize (outSize-1), outHead)

main = do
    let var2 = fst(dequeueBack(enqueueTop (enqueueTop Empty 31) 21))
    print(var2)
