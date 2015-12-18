--Метод банкира
--Представим, что за добавление нового элемента в конец или в начало очереди,
--мы получаем 3$. За примитивную операцию мы должны заплатить 1$.
--При добавлении элемента в конец или в начало очереди мы должны потратить 1$
--на добавление этого элемента. Следовательно наш доход при добавлении в 
--очередь N элементов будет равен 2N$.
--Наихудшим вариантом удаления будет такое развитие событий, при котором на
--каждой итерации нам придется переносить данные из одного стека в другой.
--Например, пусть первый стек у нас полон, а второй пуст. При удалении
--элемента из начала очереди мы тратим 1$ на удаление элемента и (N-1)$
--на копирование стека. Таким образом, при удалении одного элемента мы
--зарабатываем 2-1-(N-1) = (2-N)$, следовательно, очередь имеет линейную
--сложность
-------------------------------------------------------------------------------
--Метод физика
--Считаем, что потенциал вектора равен N+M, где N - количество элементов в
--первом стеке, а M - количество элементов во втором стеке.
--Добавление в начало или в конец очереди расходует О(1) и увеличивает
--потециал на 1, О(1)+1=О(1)
--Удаление из начала при пустом втором стеке расходует О(N) и не изменяет
--потенциал. Следовательно сложность равна О(N) - 0 = O(N) - линейная
data Queue a = Empty
             | Queue [a] [a]
               deriving (Show)

--Добавление в конец
enqueueBack :: Queue a -> a -> Queue a
enqueueBack Empty x = Queue [] [x]
enqueueBack (Queue [] []) x = Queue [] [x]
enqueueBack (Queue inStack outStack) x = Queue (x: inStack) outStack

--Добавление в начало 
enqueueTop :: Queue a -> a -> Queue a
enqueueTop Empty x = Queue [x] []
enqueueTop (Queue [] []) x = Queue [x] []
enqueueTop (Queue inStack outStack) x = Queue inStack (x: outStack)

--Удаление из начала
dequeueTop :: Queue a -> (Queue a, a)
dequeueTop Empty = error ""
dequeueTop (Queue [] []) = error ""
dequeueTop (Queue [] [x]) = (Queue [] [], x)
dequeueTop (Queue [x] []) = (Queue [] [], x)
dequeueTop (Queue (oe:ot) []) = (Queue [] (reverse ot), oe)
dequeueTop (Queue inStack (oe:ot)) = (Queue inStack ot, oe)

--Удаление из конца 
dequeueBack :: Queue a -> (Queue a, a)
dequeueBack Empty = error ""
dequeueBack (Queue [] []) = error ""
dequeueBack (Queue [x] [] ) = (Queue [] [], x)
dequeueBack (Queue [] [x] ) = (Queue [] [], x)
dequeueBack (Queue [] (oe:ot)) = (Queue (reverse ot) [], oe)
dequeueBack (Queue (oe:ot) outStack) = (Queue ot outStack, oe)

main = do
    let var2 = fst(dequeueBack(enqueueTop (enqueueTop Empty 31) 21))
    print(dequeueBack var2)