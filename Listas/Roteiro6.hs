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