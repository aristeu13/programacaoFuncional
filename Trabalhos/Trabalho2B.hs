
-- 6
-- a
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração
  | Mult (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)


avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

-- b
main = do
    print((3+12)*(15-5)^(1*3))
    print(-((6+8-5+1)*(2+6^2)))


expressao1 :: Double
expressao1 = avalia (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))


expressao2 :: Double
expressao2 = avalia (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

-- 7
data Hora 
  = PM Int Int
  | AM Int Int
  deriving (Eq, Show, Ord)

-- a
-- b
validaHoras :: Int -> Bool
validaHoras horas
  | horas >= 0 && horas <= 11 = True
  | otherwise = False

validaMinutos :: Int -> Bool
validaMinutos minutos
  | minutos >= 0 && minutos <= 59 = True
  | otherwise = False

horasDecorridas :: Hora -> Int
horasDecorridas (AM horas minutos)
  | validaHoras horas &&  validaMinutos minutos = horas
  | otherwise = undefined
horasDecorridas (PM horas minutos)
  | validaHoras horas &&  validaMinutos minutos = horas + 12
  | otherwise = undefined

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM horas minutos)
  | validaHoras horas && validaMinutos minutos = horas*60 + minutos
  | otherwise = undefined
minutosDecorridos (PM horas minutos)
  | validaHoras horas &&  validaMinutos minutos = 12*60 + horas*60 + minutos
  | otherwise = undefined


segundosDecorridos :: Hora -> Int
segundosDecorridos (AM horas minutos) = horas*60*60 + minutos*60
segundosDecorridos (PM horas minutos) = (12 + horas)*60*60 + minutos*60

-- 8



-- 9
data ArvBinInt
  = Nulo
  | No Int ArvBinInt ArvBinInt

arvDados :: ArvBinInt
arvDados =
  No
    20
    (No 10 (No 5 Nulo Nulo) (No 15 Nulo Nulo))
    ( No 30 (No 25 Nulo Nulo) (No 35 Nulo Nulo))

internos :: ArvBinInt -> [Int]
internos Nulo = []
internos (No _ Nulo Nulo) = []
internos (No info esq dir) = (info : internos (esq)) ++ internos (dir)

somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No info Nulo Nulo) = info
somaNos (No info esq dir) = info + (somaNos esq) + (somaNos dir)

pertence :: ArvBinInt -> Int -> Bool
pertence Nulo _ = False
pertence (No info Nulo Nulo) valor = if info == valor then True else False
pertence (No info esq dir) valor
  | info == valor = True
  | otherwise = pertence esq valor || pertence dir valor

-- 10
data ArvBinEA a = Vazia |
    Folha a |
    NoEA (Char, ArvBinEA a, ArvBinEA a)
    deriving (Show)


ea :: ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)



resolveArv :: Fractional a => ArvBinEA a -> a
resolveArv Vazia = 0
resolveArv (Folha x) = x
resolveArv (NoEA (op, folha1, folha2 ))
  | op == '+' = resolveArv folha1 + resolveArv folha2
  | op == '-' = resolveArv folha1 - resolveArv folha2
  | op == '*' = resolveArv folha1 * resolveArv folha2
  | op == '/' = resolveArv folha1 / resolveArv folha2
  | otherwise = undefined
