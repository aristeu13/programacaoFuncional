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
-- bolha2 :: Ord a => [a] -> [a]
-- bolha2 [] = []
-- bolha2 lst = bolhaOrd2 lst (length lst)

-- bolhaOrd2 :: Ord a => [a] -> Int -> [a]
-- bolhaOrd2 lst 0 = lst
-- bolhaOrd2 lst n = bolhaOrd2 (troca2 lst) (n-1)

troca2 :: Ord a => [a] -> [a]
troca2 [x] = [x]
troca2 (x:y:zs)
  | x > y     = y : troca2 (x:zs)
  | otherwise = x : troca2 (y:zs)
