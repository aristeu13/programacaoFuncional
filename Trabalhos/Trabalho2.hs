l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

-- a
selecao :: Ord a => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
                where x = foldr1 (minimo) xs

remove :: Ord a => a -> [a] -> [a]
remove _ [] = []
remove a (x:xs)
    | a == x = xs
    | otherwise = x:(remove a xs)

minimo :: Ord a => a -> a -> a
minimo a b
    | a > b = b
    | otherwise = a

-- b
insercao :: Ord a => [a] -> [a]
insercao = foldr insereOrd []

insereOrd :: Ord a => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y:(insereOrd x ys)

-- c
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = (quicksort (filter (s>) xs)) ++ [s] ++ (quicksort (filter (s<=) xs))

-- 2
-- varicao1
bolhaVar1 :: Ord a => [a] -> [a]
bolhaVar1 [] = []
bolhaVar1 lst = bolhaVar1Ord lst (length lst)

bolhaVar1Ord :: Ord a => [a] -> Int -> [a]
bolhaVar1Ord lst 0 = lst
bolhaVar1Ord lst n = do
    if count /= 0 then
        bolhaVar1Ord list (n-1)
        else lst
        where
            (list, count) = troca1 (lst, 0)

troca1 :: Ord a => ([a], Int) -> ([a], Int)
troca1 ([x], n) = ([x], n)
troca1 ((x:y:zs), n) = if x > y then
  addInitList (troca1 ((x:zs), n+1)) y else
  addInitList (troca1 ((y:zs), n)) x
    where
      addInitList (list, n) a = (a:list, n)

-- varicao2
bolhaVar2 :: Ord a => [a] -> [a]
bolhaVar2 [] = []
bolhaVar2 lst = bolhaVar2Ord lst (length lst)

bolhaVar2Ord :: Ord a => [a] -> Int -> [a]
bolhaVar2Ord lst 0 = lst
bolhaVar2Ord lst n = bolhaVar2Ord (troca2 lst) (n-1)

bolhaOrd [x] = [x]
bolhaOrd l = (bolhaOrd body) ++ last
    where
        lst = troca2 l
        last = splitLast lst
        body = splitBody lst

splitBody :: [a] -> [a]
splitBody lst = take (length lst - 1) lst

splitLast :: [a] -> [a]
splitLast lst = drop (length lst - 1) lst

troca2 :: Ord a => [a] -> [a]
troca2 [x] = [x]
troca2 (x:y:zs) = if x > y then
  y:(troca2 (x:zs)) else
  x:(troca2 (y:zs))

-- varicao3
bolhaVar3 :: Ord a => [a] -> [a]
bolhaVar3 [] = []
bolhaVar3 lst = bolhaVar3Ord lst (length lst)

bolhaVar3Ord :: Ord a => [a] -> Int -> [a]
bolhaVar3Ord lst 0 = lst
bolhaVar3Ord lst n = do
    if count /= 0 then
        (bolhaVar3Ord body (n-1)) ++ last
        else list
        where
            (list, count) = troca3 (lst, 0)
            last = splitLast list
            body = splitBody list

troca3 :: Ord a => ([a], Int) -> ([a], Int)
troca3 ([x], n) = ([x], n)
troca3 ((x:y:zs), n) = if x > y then
  addInitList (troca3 ((x:zs), n+1)) y else
  addInitList (troca3 ((y:zs), n)) x
    where
      addInitList (list, n) a = (a:list, n)


main = do
    print(bolhaVar1 l1)
    print(bolhaVar2 l1)
    print(bolhaVar3 l1)
    print(bolhaVar1 l2)
    print(bolhaVar2 l2)
    print(bolhaVar3 l2)

