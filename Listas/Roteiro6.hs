-- 1
paridade :: [Int] -> [Bool]
paridade lista = map even lista

-- 2
prefixos :: [String] -> [String]
prefixos l = map (take 3) l

-- 3
saudacao :: [String] -> [String]
saudacao l = map ("oi " ++) l

-- 4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar e (lh : lt)
  | e lh = lh : filtrar e lt
  | otherwise = filtrar e lt

filtrar_compr :: (a -> Bool) -> [a] -> [a]
filtrar_compr f l = [ x | x <- l, f x == True]

-- 5
pares :: [Int] -> [Int]
pares lst = filter even lst

-- 6
solucoes :: [Int] -> [Int]
solucoes lst = filter (\x -> (5*x + 6) < (x*x)) lst

-- 7
maior :: [Int] -> Int
maior lst = foldr1 max lst

-- 8
menor_min10 :: [Int] -> Int
menor_min10 lst = foldr (min) 10 lst

-- 9
junta_silabasplural :: [String] -> String
junta_silabasplural lst = foldr (++) "" lst

-- 10
bolha :: Ord a => [a] -> [a]
bolha [] = []
bolha lst = bolhaOrd lst (length lst)

bolhaOrd :: Ord a => [a] -> Int -> [a]
bolhaOrd lst 0 = lst
bolhaOrd lst n = bolhaOrd (troca lst) (n-1)

troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x:y:zs)
  | x > y     = y : troca (x:zs)
  | otherwise = x : troca (y:zs)


selecao :: Ord a => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
  where x = minimo xs

remove :: Ord a => a -> [a] -> [a]
remove _ [] = []
remove a (x:xs)
  | a==x = xs
  | otherwise = x:(remove a xs)

minimo :: Ord a => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs


insercao :: Ord a => [a] -> [a]
insercao []     = []
insercao (x:xs) = insereOrd x (insercao xs)

insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
  | x <= y = (x:y:ys)
  | otherwise = y:(insereOrd x ys)


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x|x <- xs, x < s] ++ [s] ++ quicksort [x|x <- xs, x >= s]

-- 11
bolha2 :: Ord a => [a] -> ([a], Int)
bolha2 [] = ([], 0)
bolha2 lst = bolhaOrd2 (lst, 0) (length lst)

bolhaOrd2 :: Ord a => ([a], Int) -> Int -> ([a], Int)
bolhaOrd2 (lst, c) 0 = (lst, c)
bolhaOrd2 (lst, c) n = bolhaOrd2 (troca2 (lst, c)) (n-1)

troca2 :: Ord a => ([a], Int) -> ([a], Int)
troca2 ([x], n) = ([x], n)
troca2 ((x:y:zs), n) = if x > y then
  addInitList (troca2 ((x:zs), n+1)) y else
  addInitList (troca2 ((y:zs), n+1)) x
    where
      addInitList (list, n) a = (a:list, n)


selecao2 :: Ord a => [a] -> ([a], Int)
selecao2 lista = selecaoAux lista 0

selecaoAux :: (Ord a) => [a] -> Int -> ([a], Int)
selecaoAux [] n = ([], n)
selecaoAux (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selecaoAux (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)


insercao2 :: (Ord a) => [a] -> ([a], Int)
insercao2 [] = ([], 0)
insercao2 [x] = ([x], 0)
insercao2 (h : t) =
  let (sorted_tail, n) = insercao2 t

      (lst, n1) = insereOrd2 h sorted_tail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

quicksortAux :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quicksortAux [] n _ = ([], n)
quicksortAux (x : xs) n cond =
  if (cond x)
    then add (quicksortAux xs (n + 1) cond) x
    else quicksortAux xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)


quicksort2 :: (Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (piv : xs) =
  let (left, n_L) = quicksortAux xs 0 (<= piv)
      (right, n_R) = quicksortAux xs 0 (> piv)
      (sorted_L, n1_L) = quicksort2 left
      (sorted_R, n1_R) = quicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

-- 12
bolha3 :: Ord a => [a] -> ([a], Int)
bolha3 [] = ([], 0)
bolha3 lst = bolhaOrd3 (lst, 0) (length lst)

bolhaOrd3 :: Ord a => ([a], Int) -> Int -> ([a], Int)
bolhaOrd3 (lst, c) 0 = (lst, c)
bolhaOrd3 (lst, c) n = bolhaOrd3 (troca3 (lst, c)) (n-1)

troca3 :: Ord a => ([a], Int) -> ([a], Int)
troca3 ([x], n) = ([x], n)
troca3 ((x:y:zs), n) = if x < y then --invertido
  addInitList (troca3 ((x:zs), n+1)) y else
  addInitList (troca3 ((y:zs), n+1)) x
    where
      addInitList (list, n) a = (a:list, n)


selecao3 :: Ord a => [a] -> ([a], Int)
selecao3 lista = selecaoAux2 lista 0

selecaoAux2 :: (Ord a) => [a] -> Int -> ([a], Int)
selecaoAux2 [] n = ([], n)
selecaoAux2 (x : xs) n =
  let (least, n_num) = minimo3 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selecaoAux2 (remove2 least (x : xs)) n_num) least

minimo3 :: (Ord a) => [a] -> Int -> (a, Int)
minimo3 [] _ = undefined
minimo3 [x] cont = (x, cont)
minimo3 (x : y : xs) cont
  | x < y = minimo3 (y : xs) (cont + 1) --invertido
  | otherwise = minimo3 (x : xs) (cont + 1)


insercao3 :: (Ord a) => [a] -> ([a], Int)
insercao3 [] = ([], 0)
insercao3 [x] = ([x], 0)
insercao3 (h : t) =
  let (sorted_tail, n) = insercao3 t

      (lst, n1) = insereOrd3 h sorted_tail n
   in (lst, n1)

insereOrd3 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd3 x [] n = ([x], n)
insereOrd3 x (h : t) n =
  if (x >= h)  --invertido
    then ((x : h : t), n + 1)
    else add (insereOrd3 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

quicksortAux2 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quicksortAux2 [] n _ = ([], n)
quicksortAux2 (x : xs) n cond =
  if (cond x)
    then add (quicksortAux2 xs (n + 1) cond) x
    else quicksortAux2 xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)


quicksort3 :: (Ord a) => [a] -> ([a], Int)
quicksort3 [] = ([], 0)
quicksort3 (piv : xs) =
  let (left, n_L) = quicksortAux2 xs 0 (>= piv)
      (right, n_R) = quicksortAux2 xs 0 (< piv)
      (sorted_L, n1_L) = quicksort3 left
      (sorted_R, n1_R) = quicksort3 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)


lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1