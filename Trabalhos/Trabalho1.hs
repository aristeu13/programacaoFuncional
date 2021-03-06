
-- 1
triangulo :: Float -> Float -> Float -> String

triangulo a b c
    | (a + b + c) /= 180 = "nao_triangulo"
    | a == b && b == c = "equilatero"
    | a == 90 || b == 90 || c == 90 = "retangulo"
    | a >= 90 || b >= 90 || c >= 90 = "obtuso"
    | otherwise = "simples"


-- 2
delta :: Float -> Float -> Float -> Float
delta a b c = b^2 - 4*a*c

equacao :: (Float, Float, Float) -> (Float, Float)
equacao (a, b, c)
    | a == 0 =  (0, (- c)/b)
    | otherwise = do
        let x1 = (-b + sqrt (delta a b c))/2*a
        let x2 = (-b - sqrt (delta a b c))/2*a
        (x1, x2)

-- 3

type Date = (Int, Int, Int)

calcIdade :: Date -> Date -> Int
calcIdade (d1, m1, a1) (d2, m2, a2)
  | m1 > m2 = a1 - a2
  | m1 == m2 && d1 > d2 = a1 - a2
  | otherwise = a1 - a2 -1

passagem :: Float -> Date -> Date -> Float
passagem valor dataAtual dataNascimento
  | (calcIdade dataAtual dataNascimento) < 2 = valor*0.15
  | (calcIdade dataAtual dataNascimento) <= 10 = valor*0.4
  | (calcIdade dataAtual dataNascimento) >= 70 = valor*0.5
  | otherwise = valor

-- 4
listaPadrao :: [Int]
listaPadrao = [1..15]

-- a
gera1 :: [Int]
gera1 = [ x^2 | x <- listaPadrao, odd x, x > 4, x < 14 ]

-- b
gera2 :: [(Int, Int)]
gera2 = [ (x, y) | x <- listaPadrao, x >= 1, x <= 4, y <- [x..x*2]]

-- c
gera3 :: [Int]
gera3 = [ y | x <- listaPadrao, x >= 10, x <= 15, y <- [1..x]]

-- d
gera4 :: [(Int, Int)]
gera4 = [ (x, x+1) | x <- listaPadrao, odd x]

-- e
gera5 :: [Int]
gera5 = [ x+y | (x,y) <- gera4]

-- 5

-- a
contaNegM2 :: [Int] -> Int
contaNegM2 l1 = length [ x | x <- l1, even x, x < 0]

-- b
listaNegM2 :: [Int] -> [Int]
listaNegM2 l1 = [ x | x <- l1, even x, x < 0]

-- 6
distancias :: [(Float,Float)] -> [Float]
distancias [] = []
distancias l1 = [sqrt (x^2 + y^2) | (x,y) <- l1]

-- 7
fatores :: Int -> [Int]
fatores num = [ x | x <- [1..num], (mod num x) == 0]


primos :: Int -> Int -> [Int]
primos num1 num2 = [ x | x <- [num1..num2], (fatores x) == [1, x]]


-- 8
mdcAux :: Int -> Int -> Int
mdcAux a b
  | a < b = mdcAux b a
  | b == 0 = a
  | otherwise = mdcAux b (mod a b)

mmcAux :: Int -> Int -> Int
mmcAux x y = div (x * y) (mdcAux x y)

mmc :: Int -> Int -> Int -> Int
mmc x y z = mmcAux x (mmcAux y z)

-- 9
serie :: Float -> Integer -> Float
serie x n
 | n == 1 = 1/x
 | odd n = (fromIntegral n)/x + serie x (n-1)
 | otherwise = x/(fromIntegral n) + serie x (n-1)

-- 10
fizzAux:: Integer -> [String]
fizzAux num
  | num == 1 = ["No"]
  | mod num 3 == 0 && mod num 5 == 0 = "FizzBuzz":(fizzAux (num-1))
  | mod num 3 == 0 = "Fizz":(fizzAux (num-1))
  | mod num 5 == 0 = "Buzz":(fizzAux (num-1))
  | otherwise = "No":(fizzAux (num-1))

fizzbuzz :: Integer -> [String]
fizzbuzz num = reverse(fizzAux num)

-- 11
conta_aux :: Integer -> Integer -> Integer -> Integer -> [Integer] -> (Integer, Integer)
conta_aux n1 n2 c1 c2 lista
  | lista == [] = (c1, c2)
  | n1 == (head lista) = conta_aux n1 n2 (c1+1) c2 (tail lista)
  | n2 == (head lista) = conta_aux n1 n2 c1 (c2+1) (tail lista)
  | n1 == (head lista) && n2 == (head lista) = conta_aux n1 n2 (c1+1) (c2+1) (tail lista)
  | otherwise = conta_aux n1 n2 c1 c2 (tail lista)


conta_ocorrencias :: Integer -> Integer -> [Integer] -> (Integer, Integer)
conta_ocorrencias n1 n2 lista = conta_aux n1 n2 0 0 lista

-- 12
myFilter :: Integer -> [Integer] -> Integer
myFilter _ [] = 0
myFilter x (h:t)
  | x == h = 1 + (myFilter x t)
  | otherwise = 0 + (myFilter x t)


unica_ocorrencia :: Integer -> [Integer] -> Bool
unica_ocorrencia x l = (myFilter x l) == 1

-- 13
intercala :: [Integer] -> [Integer] -> [Integer]
intercala [] [] = []
intercala a [] = a
intercala [] b = b
intercala (a : at) (b : bt) = a : b : intercala at bt

-- 14
type Contato = (String, String, String, String)

agenda :: [Contato]
agenda =  [
    ("Teste1", "Rua a", "38999884971", "aristeuneto13@gmail.com"),
    ("Teste2", "Rua b", "38999884972", "aristeuneto14@gmail.com"),
    ("Teste3", "Rua c", "38999884973", "aristeuneto15@gmail.com")
  ]

checarEmail :: String -> Contato -> Bool
checarEmail email (nome, _, _, email1)
  | email == email1 = True
  | otherwise = False

extractNome :: Contato -> String
extractNome (nome, _, _, _) = nome

buscarAux :: String -> [Contato] -> String
buscarAux email contatos
  | contatos == [] = "Email desconhecido"
  | checarEmail email (head contatos) = extractNome (head contatos)
  | otherwise = buscarAux email (tail contatos)


buscarContato :: String -> String
buscarContato email = buscarAux email agenda

-- 15

type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [
    ("Rosa",1.66, 27,'F'),
    ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58,39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S')
  ]

-- A altura média entre todas as pessoas
extractAltura :: Pessoa -> Float
extractAltura (_, altura, _, _) = altura

alturaTotal :: [Pessoa] -> Float
alturaTotal pessoas
  | pessoas == [] = 0.0
  | otherwise = extractAltura (head pessoas) + alturaTotal (tail pessoas)

alturaMedia :: Float
alturaMedia = (alturaTotal pessoas) / fromIntegral(length pessoas)

--  A idade da pessoa mais nova.
extractIdade :: Pessoa -> Int
extractIdade (_, _, idade, _) = idade

listaIdades :: [Pessoa] -> [Int]
listaIdades [] = []
listaIdades (ph:pt) = (extractIdade ph):(listaIdades pt)

idadePessoaNova :: Int
idadePessoaNova = minimum (listaIdades pessoas)

-- O nome e o estado civil da pessoa mais velha

idadePessoaVelha :: Int
idadePessoaVelha = maximum (listaIdades pessoas)

buscarInfoMaisVelhoAux :: [Pessoa] -> Int -> (String, Char)
buscarInfoMaisVelhoAux [] _ = (" ",' ')
buscarInfoMaisVelhoAux ((nome, _, idade, estado):ptail) age
  | age == idade = (nome, estado)
  | otherwise = buscarInfoMaisVelhoAux ptail age

buscarInfoMaisVelho :: (String, Char)
buscarInfoMaisVelho = buscarInfoMaisVelhoAux pessoas idadePessoaVelha

-- Todos os dados de cada pessoa com 50 anos ou mais

pessoasAcima50 :: [Pessoa]
pessoasAcima50 = [ (nome, altura, idade, estado) | (nome, altura, idade, estado)<-pessoas, idade >= 50]


-- O número de pessoas casadas com idade superior a i (ex: i = 35).

pessoasCasadasSupI :: Int -> Int
pessoasCasadasSupI i = length [ (nome, altura, idade, estado) | (nome, altura, idade, estado)<-pessoas, estado == 'C', idade > i]

-- 16
insere_ord :: Ord a => a -> [a] -> [a]
insere_ord x [] = x:[]
insere_ord x (hl:tl)
  | (x > hl) == False = x:hl:tl
  | otherwise = hl:(insere_ord x tl)

-- 17
reverteAux :: Ord a => [a] -> [a] -> [a]
reverteAux [] l2 = l2
reverteAux (h1:t1) l2 = reverteAux t1 (h1:l2)

reverte :: Ord a => [a] -> [a]
reverte lista = reverteAux lista []

-- 18

checaRepeticao :: Ord a => a -> [a] -> Bool
checaRepeticao elem [] = False
checaRepeticao elem (hl:tl)
  | elem == hl = True
  | otherwise = checaRepeticao elem tl


semRepetidosAux :: Ord a => [a] -> [a] -> [a]
semRepetidosAux [] lista2 = reverte lista2
semRepetidosAux (h1:t1) lista2
  | checaRepeticao h1 lista2 == False = semRepetidosAux t1 (h1:lista2)
  | otherwise = semRepetidosAux t1 lista2

sem_repetidos :: Ord a => [a] -> [a]
sem_repetidos lista = semRepetidosAux lista []

-- 19
disponiveis :: [Int]
disponiveis = [1,2,5,10,20,50,100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco nota = [ hl:tl | hl <- disponiveis, hl <= nota, tl <- notasTroco (nota - hl)]
