
-- 1
-- a
type Data = (Int, Int, Int)

valida :: Data -> Bool
valida date = do
    if day < 1 || day > 31 then False
    else
        if month < 1 || month > 12 then False
        else
            if year < 1 then False
            else True
    where (day, month, year) = date

-- b
bissexto :: Int -> Bool
bissexto ano = if bool1 then True 
    else (if bool2 then True else False)
    where
        bool1 = mod ano 400 == 0;
        bool2 = mod ano 4 == 0 && mod ano 100 /= 0;

lista :: [Int]
lista = [1..15]

bissextos :: [Int] -> [Int]
bissextos lista = [ x | x <- lista, bissexto x]

-- c
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

precede :: Data -> Data -> Bool
precede date1 date2 = do
    if y1 < y2 then True
    else
        if m1 < m2 then True
        else
            if d1 < d2 then True
            else False
    where
        (d1, m1, y1) = date1;
        (d2, m2, y2) = date2;

bdEmprestimo::Emprestimos
bdEmprestimo =[
    ("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")
    ]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados [] _ = []
atrasados ((codLivro, matrc, dataInicio, dataFinal, status): tailEmp) dataAtual
    | precede dataAtual dataFinal == True && status == "aberto" = (codLivro, matrc, dataInicio, dataFinal, status):(atrasados tailEmp dataAtual)
    | otherwise = atrasados tailEmp dataAtual

-- d
type Par = (Int, Int)

passo :: Par -> Par
passo (x, y) = (y, x+y)

auxfibo :: Int -> Par
auxfibo 1 = (1, 1)
auxfibo n = passo (auxfibo (n-1))

fibo :: Int -> Int
fibo n = x
    where (x, _) = auxfibo n

-- e
prodIntervalo :: Par -> Int
prodIntervalo (m, n)
    | m == n = n
    | otherwise = m*prodIntervalo(m+1, n)

fatorial :: Int -> Int
fatorial x = prodIntervalo (1,x)

-- 2
-- a
valida2 :: Data -> Bool
valida2 date = do
    let {(day, month, year) = date} in if day < 1 || day > 31 then False
    else
        if month < 1 || month > 12 then False
        else
            if year < 1 then False
            else True

-- b
bissexto2 :: Int -> Bool
bissexto2 ano = let {bool1 = mod ano 400 == 0; bool2 = mod ano 4 == 0 && mod ano 100 /= 0;} in if bool1 then True 
    else (if bool2 then True else False)

bissextos2 :: [Int] -> [Int]
bissextos2 lista = [ x | x <- lista, bissexto x]

-- c
precede2 :: Data -> Data -> Bool
precede2 date1 date2 = do
    let {(d1, m1, y1) = date1;(d2, m2, y2) = date2;} in if y1 < y2 then True
    else
        if m1 < m2 then True
        else
            if d1 < d2 then True
            else False

bdEmprestimo2::Emprestimos
bdEmprestimo2 =[
    ("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")
    ]

atrasados2 :: Emprestimos -> Data -> Emprestimos
atrasados2 [] _ = []
atrasados2 ((codLivro, matrc, dataInicio, dataFinal, status): tailEmp) dataAtual
    | precede dataAtual dataFinal == True && status == "aberto" = (codLivro, matrc, dataInicio, dataFinal, status):(atrasados tailEmp dataAtual)
    | otherwise = atrasados2 tailEmp dataAtual

-- d
passo2 :: Par -> Par
passo2 (x, y) = (y, x+y)

auxfibo2 :: Int -> Par
auxfibo2 1 = (1, 1)
auxfibo2 n = passo (auxfibo (n-1))

fibo2 :: Int -> Int
fibo2 n = let {(x, _) = auxfibo n;} in x

-- e
prodIntervalo2 :: Par -> Int
prodIntervalo2 (m, n)
    | m == n = n
    | otherwise = m*prodIntervalo2(m+1, n)

fatorial2 :: Int -> Int
fatorial2 x = prodIntervalo2 (1,x)

-- 3
{-
    1)
    (λ x.2*x + 1 ) 3
    2*3 + 1 
    6 + 1
    7

    2)
    (λ xy. x-y) 5 7
    5-7
    -2

    3)
    (λ yx. x-y) 5 7
    7-5
    2

    4)
    (λ xy. x-y) (λ z. z/2)
    (λ y. (λ z. z/2)-y)

    5)
    (λ xy. x-y) ((λ z. z/2) 6 ) 1
    (λ xy. x-y) (6/2) 1
    (λ xy. x-y) 3 1
    3-1
    2

    6)
    (λ x. λ y. - x y) 9 4
    (λ x. - x 9) 4
    - 4 9
    -5

    7)
    (λ x. xx) (λ y. y)
    (λ y. y)(λ y. y)
    (λ y. y)
-}

-- 4
main = do
    print((\x -> x + 3) 5)
    print((\x -> \y -> x * y + 5) 3 4)
    print((\(x,y) -> x * y^2) (3,4))
    print((\(x,y,_) -> x * y^2) (3,4,2))
    print((\xs -> zip xs [1,2,3]) [4,5,6])

-- 5
main2 = do
    -- a
    print( (\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w) 5)
    -- b
    print( ((\f -> (\x -> f(f x))) (\y -> (y * y))) 3)
    -- c
    print( ((\f -> (\x -> f(f x)))(\y -> (y + y))) 5)
    -- d
    print(((\x -> (\y -> x + y) 5) ((\y -> y - 3) 7)))
    -- e
    print((((\f -> (\x -> f(f(f x)))) (\y -> (y * y))) 2))
    -- f
    print((\x -> \y -> x + ((\x -> x - 3) y)) 5 6)