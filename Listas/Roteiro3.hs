

customOr1 :: Bool -> Bool -> Bool
customOr1 _ True = True
customOr1 True _ = True
customOr1 _ _ = False

customOr2 :: Bool -> Bool -> Bool
customOr2 True True = True
customOr2 True False = True
customOr2 False True = True
customOr2 False False = False

customOr3 :: Bool -> Bool -> Bool
customOr3 True _ = True
customOr3 False b = b

customOr4 :: Bool -> Bool -> Bool
customOr4 bool1 bool2
    | bool1 == True = True
    | bool2 == True = True
    | otherwise     = False


customOr5 :: Bool -> Bool -> Bool
customOr5 bool1 bool2
    | bool1 == False && bool2 == False = False
    | otherwise                        = True


type Ponto = (Float, Float, Float)
distanciaEspaco :: Ponto -> Ponto -> Float
distanciaEspaco ponto1 ponto2 = do
    let (x1, y1, z1) = ponto1
    let (x2, y2, z2) = ponto2
    let x = (x1 - x2) ^ 2
    let y = (y1 - y2) ^ 2
    let z = (z1 - z2) ^ 2
    sqrt (x+y+z)


main = do
    print(1:[2,3,4])
    print('a':['b','c','d'])
    print(head [1,2,3])
    print(tail [1,2,3])
    print([1,5,2,3]!!1)
    print([1,5,2,3]!!3)
    print(elem 2 [1,5,2,3])
    print(take 2 [1,5,2,3,7])
    print(drop 2 [1,5,2,3,7])
    print([1,2] ++ [3,4])
    print([1..10])
    print([7,6..3])
    print(['b'..'g'])
    print(take 5 [1,3..])
    print(sum [1..10])
    print(maximum [1,5,2,3,7])
    print(minimum [1,5,2,3,7])


fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

fatorial2 :: Int -> Int
fatorial2 n
    | n == 0 = 1
    | otherwise = fatorial2(n-1)*n

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-2) + fibo (n-1)

n_tri :: Int -> Int
n_tri 0 = 0
n_tri 1 = 1
n_tri n = n + n_tri(n-1)

type Par = (Int, Int)

passo :: Par -> Par
passo (x, y) = (y, x+y)

auxfibo :: Int -> Par
auxfibo 1 = (1, 1)
auxfibo n = passo (auxfibo (n-1))

fibo2 :: Int -> Int
fibo2 n = do
    let (x, y) = auxfibo n
    x

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 n = 2*potencia2(n-1)

prodIntervalo :: Par -> Int
prodIntervalo (m, n)
    | m == n = n
    | otherwise = m*prodIntervalo(m+1, n)

fatorial3 :: Int -> Int
fatorial3 x = prodIntervalo (1,x)

resto_div :: Int -> Int -> Int
resto_div m n
    | m < n = m
    | otherwise = resto_div (m-n) n

aux_quo :: Int -> Int -> Int -> Int
aux_quo m n q
    | m < n = q
    | otherwise = aux_quo (m-n) n (q + 1)

div_inteira :: Int -> Int -> Int
div_inteira m n = aux_quo m n 0


mdc :: Int -> Int -> Int
mdc m 0 = m
mdc m n = mdc n (mod m n)

mdc2 :: Int -> Int -> Int
mdc2 m n
    | n == 0 = m
    | otherwise = mdc2 n (mod m n)


binomial :: Int -> Int -> Int
binomial n 0 = 1
binomial n k
    | k > n = 0
    | n == k = 1
    | otherwise = binomial (n-1) k + binomial (n-1) (k-1)

binomial2 :: Int -> Int -> Int
binomial2 n k
    | k > n = 0
    | k == 0 = 1
    | n == k = 1
    | otherwise = binomial (n-1) k + binomial (n-1) (k-1)


main2 = do
    print([5,4..1])
    print(['a','c'..'e'])
    print([1,4..16])
    print(zip [1, -2..(-11)] [1,5..17])

lista :: Int -> Int -> [Int]
lista a b
    | a == b = [a]
    | a > b = []
    | otherwise = a:(lista (a+1) b)

listaPares :: Int -> Int -> [Int]
listaPares a b
    | a == b = []
    | a + 1 == b = []
    | a > b = []
    | mod a 2 == 0 = listaPares (a+1) b
    | otherwise = (a+1):(listaPares (a+1) b)
