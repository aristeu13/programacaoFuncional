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