-- 1
main = do
    let lst1 = [x*2 | x <- [1..10], x*2 >= 12]
    let lst2 = [ x | x <- [50..100], mod x 7 == 3]
    let lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
    let lst4=[(x,y)| x <- [1..4], y <- [x..5]]
    print(lst1)
    print(lst2)
    print(lst3)
    print(lst4)

-- 2
quadrados :: Int -> Int -> [Int]
quadrados num1 num2 = [ x*x | x <-[num1..num2] ]

-- 3
seleciona_impares :: [Int] -> [Int]
seleciona_impares lista = [ x | x <- lista, odd x]

-- 4
tabuada :: Int -> [Int]
tabuada num = [ x*num | x <- [1..10]]

-- 5
bissexto :: Int -> Bool
bissexto ano = if mod ano 400 == 0 then True 
    else (if mod ano 4 == 0 && mod ano 100 /= 0 then True else False)


bissextos :: [Int] -> [Int]
bissextos lista = [ x | x <- lista, bissexto x]

-- 6
sublistas :: [[Int]] -> [Int]
sublistas lista = [ x | sub <- lista, x <- sub]

-- 7


type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

precede :: Data -> Data -> Bool
precede date1 date2 = do
    let (d1, m1, y1) = date1
    let (d2, m2, y2) = date2
    if y1 < y2 then True
    else
        if m1 < m2 then True
        else
            if d1 < d2 then True
            else False

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

-- 8
npares :: [Int] -> Int
npares [] = 0
npares (num:tailNums)
    | even num = 1+npares tailNums
    | otherwise = npares tailNums

-- 9

prodTotal :: [Int] ->Int
prodTotal [] = 1
prodTotal (num:tailNums) = num*prodTotal(tailNums)

-- 10
comprime :: [[Int]] -> [Int]
comprime [] = []
comprime (hl:tl) = hl ++ comprime tl

-- 11
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (hl:tl) = 1 + tamanho tl

-- 12
checaRepeticao :: Ord a => a -> [a] -> Bool
checaRepeticao elem [] = False
checaRepeticao elem (hl:tl)
  | elem == hl = True
  | otherwise = checaRepeticao elem tl

uniaoNRec :: Ord a => [a] -> [a] -> [a]
uniaoNRec l1 l2 = [ x | x <- l1] ++ [ y | y <- l2, not (checaRepeticao y l1)]

-- 13
uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 l1 [] = l1
uniaoRec2 l1 (x : xs)
  | checaRepeticao x l1 == True = uniaoRec2 l1 xs
  | otherwise = uniaoRec2 (l1 ++ [x]) xs
