l1 :: [Integer]
l1 = [1 .. 1000]

l2 :: [Integer]
l2 = [1000, 999 .. 1]

l3 :: [Integer]
l3 = l1 ++ [0]

l4 :: [Integer]
l4 = [0] ++ l2

l5 :: [Integer]
l5 = l1 ++ [0] ++ l2

l6 :: [Integer]
l6 = l2 ++ [0] ++ l1

l7 :: [Integer]
l7 = l2 ++ [0] ++ l2

x1 :: [Integer]
x1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

x2 :: [Integer]
x2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

x3 :: [Integer]
x3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x4 :: [Integer]
x4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

x5 :: [Integer]
x5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

x6 :: [Integer]
x6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

x7 :: [Integer]
x7 = [20, 8, 2, 11, 13, 3, 7, 18, 14, 4, 16, 10, 15, 1, 9, 17, 19, 12, 5, 6]

-- Exercicio 1 --
-- a --
selecao :: Ord a => [a] -> [a]
selecao [] = []
selecao [x] = [x]
selecao xs = [x] ++ selecao (remove x xs)
  where
    x = foldr1 (minimo) xs

remove :: Ord a => a -> [a] -> [a]
remove _ [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)

minimo :: Ord a => a -> a -> a
minimo a b
  | a > b = b
  | otherwise = a

-- b --
insercao :: Ord a => [a] -> [a]
insercao = foldr insereOrd []

insereOrd :: Ord a => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y : ys)
  | x <= y = (x : y : ys)
  | otherwise = y : (insereOrd x ys)

-- c --
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (s : xs) = (quicksort (filter (s >) xs)) ++ [s] ++ (quicksort (filter (s <=) xs))

-- Exercicio 2 --
-- varicao 1 --
bolhaVar1 :: Ord a => [a] -> [a]
bolhaVar1 [] = []
bolhaVar1 lst = bolhaVar1Ord lst (length lst)

bolhaVar1Ord :: Ord a => [a] -> Int -> [a]
bolhaVar1Ord lst 0 = lst
bolhaVar1Ord lst n = do
  if count /= 0
    then bolhaVar1Ord list (n -1)
    else lst
  where
    (list, count) = troca1 (lst, 0)

troca1 :: Ord a => ([a], Int) -> ([a], Int)
troca1 ([x], n) = ([x], n)
troca1 ((x : y : zs), n) =
  if x > y
    then addInitList (troca1 ((x : zs), n + 1)) y
    else addInitList (troca1 ((y : zs), n + 1)) x
  where
    addInitList (list, n) a = (a : list, n)

-- varicao 2--
bolhaVar2 :: Ord a => [a] -> [a]
bolhaVar2 [] = []
bolhaVar2 lst = bolhaVar2Ord lst (length lst)

bolhaVar2Ord :: Ord a => [a] -> Int -> [a]
bolhaVar2Ord lst 0 = lst
bolhaVar2Ord lst n = bolhaVar2Ord (troca2 lst) (n -1)

bolhaOrd :: Ord a => [a] -> [a]
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
troca2 (x : y : zs) =
  if x > y
    then y : (troca2 (x : zs))
    else x : (troca2 (y : zs))

-- varicao 3 --
bolhaVar3 :: Ord a => [a] -> [a]
bolhaVar3 [] = []
bolhaVar3 lst = bolhaVar3Ord lst (length lst)

bolhaVar3Ord :: Ord a => [a] -> Int -> [a]
bolhaVar3Ord lst 0 = lst
bolhaVar3Ord lst n = do
  if count /= 0
    then (bolhaVar3Ord body (n -1)) ++ last
    else list
  where
    (list, count) = troca3 (lst, 0)
    last = splitLast list
    body = splitBody list

troca3 :: Ord a => ([a], Int) -> ([a], Int)
troca3 ([x], n) = ([x], n)
troca3 ((x : y : zs), n) =
  if x > y
    then addInitList (troca3 ((x : zs), n + 1)) y
    else addInitList (troca3 ((y : zs), n + 1)) x
  where
    addInitList (list, n) a = (a : list, n)

-- cont --
-- variacao 1 --
bolha1Cont :: (Ord a) => [a] -> ([a], Int)
bolha1Cont [] = ([], 0)
bolha1Cont list = format (bolhaAuxCont (list, -1, 0) (length list))
  where
    format (l, _, c) = (l, c)

trocaCont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
trocaCont ([x], flag, n) = ([x], flag, n)
trocaCont ((x : y : xs), flag, n) =
  if x > y
    then add (trocaCont ((x : xs), 1, n + 1)) y
    else add (trocaCont ((y : xs), flag, n + 1)) x
  where
    add (l, f, c) e = (e : l, f, c)

bolhaAuxCont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bolhaAuxCont (l, flag, c) 0 = (l, flag, c)
bolhaAuxCont (l, flag, c) n
  | flag == 0 = (l, flag, c)
  | otherwise = bolhaAuxCont (trocaCont (l, 0, c)) (n -1)

-- variacao 2 --
bolha2Cont :: (Ord a) => [a] -> ([a], Int)
bolha2Cont [] = ([], 0)
bolha2Cont lst =
  let add (l, c) e = (e : l, c)

      troca ([x], c) = ([x], c)
      troca ((x : y : xs), c) =
        if x > y
          then add (troca (x : xs, c + 1)) y
          else add (troca (y : xs, c + 1)) x

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bolha :: (Ord a) => ([a], Int) -> ([a], Int)
      bolha ([x], c) = ([x], c)
      bolha (l, c) = (proxEtapa ++ ultElem, rec_c)
        where
          (listaTrocada, c1) = (troca (l, c))
          (paraTrocar, ultElem) = split listaTrocada
          (proxEtapa, rec_c) = bolha (paraTrocar, c1)
   in bolha (lst, 0)

-- variacao 3 --
bolha3Cont :: (Ord a) => [a] -> ([a], Int)
bolha3Cont [] = ([], 0)
bolha3Cont l =
  let add (l, f, c) y = (y : l, f, c)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)
      format (l, _, c) = (l, c)

      troca ([x], flag, c) = ([x], flag, c)
      troca ((x : y : xs), flag, c) =
        if x > y
          then add (troca ((x : xs), 1, c + 1)) y
          else add (troca ((y : xs), flag, c + 1)) x

      bolha ([x], flag, c) = ([x], flag, c)
      bolha (lst, flag, c)
        | n_flag == 0 = (lst, flag, c)
        | otherwise = (proxEtapa ++ ultElem, 0, rec_c)
        where
          (listaTrocada, n_flag, c1) = troca (lst, flag, c)
          (paraTrocar, ultElem) = split listaTrocada
          (proxEtapa, _, rec_c) = bolha (paraTrocar, 0, c1)
   in format (bolha (l, -1, 0))

main :: IO ()
main = do
  print (bolhaVar1 l1)
  print (bolhaVar2 l1)
  print (bolhaVar3 l1)
  print (bolhaVar1 l2)
  print (bolhaVar2 l2)
  print (bolhaVar3 l2)

-- Exercicio 3--
-- variacao 1 --
selecaoVar1 :: Ord a => [a] -> [a]
selecaoVar1 [] = []
selecaoVar1 xs = x : (selecao (remove1 x xs))
  where
    x = minimo1 xs

remove1 :: Ord a => a -> [a] -> [a]
remove1 _ [] = []
remove1 a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)

minimo1 :: Ord a => [a] -> a
minimo1 [] = undefined
minimo1 [x] = x
minimo1 (x : xs)
  | x <= (minimo1 xs) = x
  | otherwise = minimo1 xs

-- variacao 2 --
selecaoVar2 :: Ord a => [a] -> [a]
selecaoVar2 [] = []
selecaoVar2 [x] = [x]
selecaoVar2 (x : xs) = e : (selecaoVar2 lst)
  where
    (e, lst) = remove_menor (x, xs)

remove_menor :: (Ord a) => (a, [a]) -> (a, [a])
remove_menor (m, [x]) = if x < m then (x, [m]) else (m, [x])
remove_menor (menor, (x : xs))
  | x < menor = add menor (remove_menor (x, xs))
  | otherwise = add x (remove_menor (menor, xs))
  where
    add a (n, l) = (n, a : l)

-- cont --
selecaoVarCont :: (Ord a) => [a] -> ([a], Int)
selecaoVarCont [] = ([], 0)
selecaoVarCont [x] = ([x], 0)
selecaoVarCont (x : xs) =
  let (least, novoUlt, cont) = remove_menorCont (x, xs, 0)

      (proxima_etapa, nCont) = selecaoVarCont novoUlt
   in (least : proxima_etapa, cont + nCont)

remove_menorCont :: (Ord a) => (a, [a], Int) -> (a, [a], Int)
remove_menorCont (m, [x], c) = if x < m then (x, [m], c + 1) else (m, [x], c + 1)
remove_menorCont (menor, (x : xs), c1)
  | x < menor = add menor (remove_menorCont (x, xs, c1 + 1))
  | otherwise = add x (remove_menorCont (menor, xs, c1 + 1))
  where
    add a (n, l, c) = (n, a : l, c)

-- 4 --
-- variacao 1 --

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

-- variacao 2 --
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

-- cont -- 
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


-- Ex5 --
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (a : as) (b : bs)
  | a > b = b : (merge (a : as) bs)
  | otherwise = a : (merge as (b : bs))

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort lst =
  let left = mergeSort (take ((length lst) `div` 2) lst)
      right = mergeSort (drop ((length lst) `div` 2) lst)
   in merge left right

-- Bucket Sort
sortIntoBuckets :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
sortIntoBuckets num k m n [bucket] =
  if ((num * k) `div` m) <= n
    then [num : bucket]
    else [bucket]
sortIntoBuckets num k m n (bucket : buckets)
  | ((num * k) `div` m) <= n = (num : bucket) : buckets
  | otherwise = bucket : (sortIntoBuckets num k m (n + 1) buckets)

bucketSort :: [Int] -> [Int]
bucketSort [] = []
bucketSort [x] = [x]
bucketSort l1 =
  let k = length l1
      m = foldr1 (max) l1
      buckets = [[] | _ <- [1 .. k]]
      newBuckets = foldr (\x -> sortIntoBuckets x k m 1) buckets l1
      sortedBuckets = map (mergeSort) newBuckets
      finalList = foldr1 (++) sortedBuckets
   in finalList