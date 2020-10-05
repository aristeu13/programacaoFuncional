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

-- 3
-- variacao1
selecaoVar1 :: Ord a => [a] -> [a]
selecaoVar1 [] = []
selecaoVar1 xs = x:(selecao (remove1 x xs))
  where x = minimo1 xs

remove1 :: Ord a => a -> [a] -> [a]
remove1 _ [] = []
remove1 a (x:xs)
  | a==x = xs
  | otherwise = x:(remove a xs)

minimo1 :: Ord a => [a] -> a
minimo1 [] = undefined
minimo1 [x] = x
minimo1 (x:xs)
  | x <= (minimo1 xs) = x
  | otherwise = minimo1 xs

-- variacao2
selecaoVar2 :: Ord a => [a] -> [a]
selecaoVar2 [] = []
selecaoVar2 [x] = [x]
selecaoVar2 (x:xs) = e:(selecaoVar2 lst)
  where (e, lst) = remove_menor (x, xs)

remove_menor :: (Ord a) => (a, [a]) -> (a, [a])
remove_menor (m, [x]) = if x < m then (x, [m]) else (m, [x])
remove_menor (menor, (x : xs))
  | x < menor = add menor (remove_menor (x, xs))
  | otherwise = add x (remove_menor (menor, xs))
  where
    add a (n, l) = (n, a : l)

-- variacao3
selecaoVar3 :: (Ord a) => [a] -> ([a], Int)
selecaoVar3 [] = ([], 0)
selecaoVar3 [x] = ([x], 0)
selecaoVar3 (x : xs) =
  let (least, novoUlt, cont) = remove_menor3 (x, xs, 0)

      (proxima_etapa, nCont) = selecaoVar3 novoUlt
   in (least : proxima_etapa, cont + nCont)

remove_menor3 :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
remove_menor3 (m, [x], c) = if x < m then (x, [m], c + 1) else (m, [x], c + 1)
remove_menor3 (menor, (x : xs), c1)
  | x < menor = add menor (remove_menor3 (x, xs, c1 + 1))
  | otherwise = add x (remove_menor3 (menor, xs, c1 + 1))
  where
    add a (n, l, c) = (n, a : l, c)

-- 4
-- variacao1

divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide x [e] = if e < x then ([e], []) else ([], [e])
divide x (e : es)
  | e < x = addEsq e (divide x es)
  | otherwise = addDir e (divide x es)
  where
    addEsq a (l, r) = (a : l, r)
    addDir a (l, r) = (l, a : r)

quickSortVar1 :: (Ord a) => [a] -> [a]
quickSortVar1 [] = []
quickSortVar1 (piv : xs) =
  let (left, right) = divide piv xs
   in (quickSortVar1 left) ++ [piv] ++ (quickSortVar1 right)

-- variacao2
quickSortVar2 :: (Ord a) => [a] -> [a]
quickSortVar2 [] = []
quickSortVar2 lst =
  let firstThree = take 3 lst
      piv =
        if length (firstThree) < 3
          then firstThree !! 0
          else foldr1 (min) (firstThree)

      deletaPrimOcorrencia _ [] = []
      deletaPrimOcorrencia x (y : ys)
        | x == y = ys
        | otherwise = y : deletaPrimOcorrencia x ys

      (left, right) = divide piv (deletaPrimOcorrencia piv lst)
   in (quickSortVar2 left) ++ [piv] ++ (quickSortVar2 right)

-- variacao3 contagem
divideCont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divideCont _ [] n = ([], [], n)
divideCont x [e] n =
  if e < x
    then ([e], [], n + 1)
    else ([], [e], n + 1)
divideCont x (e : es) n
  | e < x = addEsq e (divideCont x es (n + 1))
  | otherwise = addDir e (divideCont x es (n + 1))
  where
    addEsq a (l, r, c) = (a : l, r, c)
    addDir a (l, r, c) = (l, a : r, c)

quickSortVar3 :: (Ord a) => [a] -> ([a], Int)
quickSortVar3 [] = ([], 0)
quickSortVar3 (piv : xs) =
  let (left, right, n) = divideCont piv xs 0

      (sortedL, n_L) = quickSortVar3 left
      (sortedR, n_R) = quickSortVar3 right
   in (sortedL ++ [piv] ++ sortedR, n + n_L + n_R)

quickSortVar4 :: (Ord a) => [a] -> ([a], Int)
quickSortVar4 [] = ([], 0)
quickSortVar4 lst =
  let piv = foldr1 (min) (take 3 lst)

      deleteFrstOc :: (Ord a) => a -> [a] -> Int -> ([a], Int)
      deleteFrstOc _ [] n = ([], n)
      deleteFrstOc x (y : ys) n
        | x == y = (ys, n + 1)
        | otherwise = add y (deleteFrstOc x ys (n + 1))
        where
          add e (l, c) = (e : l, c)

      (novoUlt, checks) = deleteFrstOc piv lst 0

      (left, right, n1) = divideCont piv novoUlt 0
      (sortedL, n_L) = quickSortVar4 left
      (sortedR, n_R) = quickSortVar4 right
   in (sortedL ++ [piv] ++ sortedR, n1 + n_L + n_R + checks + 3)
