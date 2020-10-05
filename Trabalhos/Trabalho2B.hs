
-- 1
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

main = do
    print((3+12)*(15-5)^(1*3))

expressao1 :: Exp Integer
expressao1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))
