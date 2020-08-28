dobro :: Float -> Float
dobro x = x*2

quadriplicar :: Float -> Float
quadriplicar x = (dobro x) * 2

hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt (x^2 + y^2)

distDoisPontos :: Float -> Float -> Float -> Float -> Float
distDoisPontos x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

main = do
    print(fst (2,5))
    print(snd (5, "Bom dia"))
    print((1,1) == (1,1))
    print((1,1) /= (1,1))
    print((1,1) < (1,2))
    print((2,1) < (1,2))
    -- print((1,2,3) < (1,2))
    print("azul" < "verde")
    print("azul" < "amarelo")
    print((1,2,3) == (,,) 1 2 3)

conversao :: Float -> (Float, Float, Float)
conversao reais = (reais, reais*3.96, reais*4.45)

bissexto :: Int -> Bool
bissexto ano = if mod ano 400 == 0 then True 
    else (if mod ano 4 == 0 && mod ano 100 /= 0 then True else False)

extractYear :: Data -> Int
extractYear (_, _, year) = year

type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 ano = do
    bissexto (extractYear ano)

valida :: Data -> Bool
valida date = do
    let (day, month, year) = date
    if day < 1 || day > 31 then False
    else
        if month < 1 || month > 12 then False
        else
            if year < 1 then False
            else True


precede :: Data -> Data -> Bool
precede date1 date2 = do
    let (d1, m1, y1) = date1
    let (d2, m2, y2) = date2
    if valida date1 == False || valida date2 == False then False
    else
        if y1 < y2 then True
        else
            if m1 < m2 then True
            else
                if d1 < d2 then True
                else False

type Livro = (String, String, String, String, Data)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

verificaEmprestimo :: Emprestimo -> Data -> Bool
verificaEmprestimo emp date = do
    let (_, _, dataDoEmp, dataDeDev, sit) = emp
    if precede date dataDeDev then True
    else 
        if sit == "fechado" then True
        else False