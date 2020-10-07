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
  print ((3 + 12) * (15 -5) ^ (1 * 3))
  print (- ((6 + 8 -5 + 1) * (2 + 6 ^ 2)))

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
  | validaHoras horas && validaMinutos minutos = horas
  | otherwise = undefined
horasDecorridas (PM horas minutos)
  | validaHoras horas && validaMinutos minutos = horas + 12
  | otherwise = undefined

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM horas minutos)
  | validaHoras horas && validaMinutos minutos = horas * 60 + minutos
  | otherwise = undefined
minutosDecorridos (PM horas minutos)
  | validaHoras horas && validaMinutos minutos = 12 * 60 + horas * 60 + minutos
  | otherwise = undefined

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM horas minutos) = horas * 60 * 60 + minutos * 60
segundosDecorridos (PM horas minutos) = (12 + horas) * 60 * 60 + minutos * 60

-- 8
type Data = (Int, Int, Int)

data Contato = Nome String | Fone String
  deriving (Eq, Show)

data Mensagem = Msg Contato String Data Hora String
  deriving (Show)

listaMsg =
  [ (Msg (Nome "Augusto Costa") "Mensagem 1" (13, 09, 20) (AM 10 30) "WhatsApp"),
    (Msg (Fone "3232-3232") "Mensagem 2" (13, 09, 20) (AM 10 31) "WhatsApp"),
    (Msg (Nome "Ana Paula Silva") "Mensagem 3" (13, 08, 20) (AM 10 32) "LinkedIn"),
    (Msg (Nome "Nome1") "Mensagem 4" (13, 09, 20) (AM 10 33) "WhatsApp"),
    (Msg (Nome "Nome2") "Mensagem 5" (13, 09, 20) (AM 10 34) "LinkedIn"),
    (Msg (Nome "Nome3") "Mensagem 6" (13, 09, 20) (AM 10 35) "LinkedIn"),
    (Msg (Nome "Nome4") "Mensagem 7" (13, 09, 20) (AM 10 36) "WhatsApp"),
    (Msg (Nome "Nome5") "Mensagem 8" (13, 09, 20) (AM 10 37) "LinkedIn"),
    (Msg (Nome "Nome6") "Mensagem 9" (13, 09, 20) (AM 10 38) "LinkedIn"),
    (Msg (Nome "Nome7") "Mensagem 10" (13, 09, 20) (AM 10 39) "WhatsApp"),
    (Msg (Nome "Nome7") "Mensagem 11" (13, 09, 20) (AM 10 40) "LinkedIn"),
    (Msg (Nome "Nome9") "Mensagem 12" (13, 09, 20) (AM 10 41) "Facebook"),
    (Msg (Nome "Nome11") "Mensagem 13" (13, 09, 20) (AM 10 42) "WhatsApp"),
    (Msg (Nome "Nome12") "Mensagem 14" (13, 09, 20) (AM 10 43) "Facebook"),
    (Msg (Nome "Nome10") "Mensagem 15" (14, 09, 20) (AM 10 44) "Facebook"),
    (Msg (Nome "Nome13") "Mensagem 16" (14, 09, 20) (AM 10 45) "Facebook"),
    (Msg (Nome "Nome14") "Mensagem 17" (14, 09, 20) (AM 10 46) "LinkedIn"),
    (Msg (Nome "Nome15") "Mensagem 18" (14, 09, 20) (AM 10 47) "LinkedIn"),
    (Msg (Nome "Nome16") "Mensagem 19" (14, 09, 20) (AM 10 48) "Facebook"),
    (Msg (Nome "Nome17") "Mensagem 20" (14, 09, 20) (AM 10 49) "LinkedIn"),
    (Msg (Nome "Nome18") "Mensagem 21" (14, 09, 20) (AM 10 51) "LinkedIn"),
    (Msg (Nome "Nome19") "Mensagem 22" (14, 09, 20) (AM 10 52) "LinkedIn"),
    (Msg (Nome "Nome20") "Mensagem 23" (14, 09, 20) (AM 10 53) "WhatsApp"),
    (Msg (Nome "Nome21") "Mensagem 24" (14, 09, 20) (AM 10 54) "Facebook"),
    (Msg (Nome "Nome22") "Mensagem 25" (14, 09, 20) (AM 10 55) "WhatsApp"),
    (Msg (Nome "Nome23") "Mensagem 26" (14, 09, 20) (AM 10 56) "Facebook"),
    (Msg (Nome "Nome24") "Mensagem 27" (14, 09, 20) (AM 10 57) "LinkedIn"),
    (Msg (Nome "Nome25") "Mensagem 28" (14, 09, 20) (AM 10 58) "WhatsApp"),
    (Msg (Nome "Nome26") "Mensagem 29" (14, 09, 20) (AM 10 59) "Facebook"),
    (Msg (Nome "Nome27") "Mensagem 30" (14, 09, 20) (AM 11 00) "LinkedIn")
  ]

-- b
bolha :: [Mensagem] -> [Mensagem]
bolha [] = []
bolha list = bolhaOrd list (length list)

bolhaOrd :: [Mensagem] -> Int -> [Mensagem]
bolhaOrd lst 0 = lst
bolhaOrd lst n = bolhaOrd (troca lst) (n -1)

troca :: [Mensagem] -> [Mensagem]
troca [x] = [x]
troca (msg1 : msg2 : tail) =
  let checa (Msg (Nome _) _ _ _ _) (Msg (Fone _) _ _ _ _) = True
      checa (Msg (Fone _) _ _ _ _) (Msg (Nome _) _ _ _ _) = False
      checa (Msg (Nome nome1) _ _ _ _) (Msg (Nome nome2) _ _ _ _) = if nome1 > nome2 then True else False
   in if checa msg1 msg2
        then msg2 : troca (msg1 : tail)
        else msg1 : troca (msg2 : tail)

-- c
precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
  | y1 > y2 = False
  | y1 == y2 && m1 > m2 = False
  | y1 == y2 && m1 == m2 && d1 > d2 = False
  | otherwise = True

precedeMsg :: Mensagem -> Mensagem -> Bool
precedeMsg (Msg _ _ data1 hora1 _) (Msg _ _ data2 hora2 _)
  | data1 == data2 = (minutosDecorridos hora1) < (minutosDecorridos hora2)
  | otherwise = precede data1 data2

quicksort :: [Mensagem] -> [Mensagem]
quicksort [] = []
quicksort (a : xs) =
  (quicksort [x | x <- xs, (precedeMsg x a) == False])
    ++ [a]
    ++ (quicksort [x | x <- xs, (precedeMsg x a) == True])

-- d
duasUltimaMensagens :: Contato -> [Mensagem] -> [Mensagem]
duasUltimaMensagens contact mensagens = take 2 [(Msg c m d h a) | (Msg c m d h a) <- msgOrd, c == contact]
  where
    msgOrd = quicksort mensagens

-- 9
data ArvBinInt
  = Nulo
  | No Int ArvBinInt ArvBinInt

arvDados :: ArvBinInt
arvDados =
  No
    20
    (No 10 (No 5 Nulo Nulo) (No 15 Nulo Nulo))
    (No 30 (No 25 Nulo Nulo) (No 35 Nulo Nulo))

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
data ArvBinEA a
  = Vazia
  | Folha a
  | NoEA (Char, ArvBinEA a, ArvBinEA a)
  deriving (Show)

ea :: ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

resolveArv :: Fractional a => ArvBinEA a -> a
resolveArv Vazia = 0
resolveArv (Folha x) = x
resolveArv (NoEA (op, folha1, folha2))
  | op == '+' = resolveArv folha1 + resolveArv folha2
  | op == '-' = resolveArv folha1 - resolveArv folha2
  | op == '*' = resolveArv folha1 * resolveArv folha2
  | op == '/' = resolveArv folha1 / resolveArv folha2
  | otherwise = undefined
