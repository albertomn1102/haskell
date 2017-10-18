module Exe where
import Data.Char
import Data.List 
--DIVIDIR EM ARQUIVOS PELOS "module xxx where"

--module Cap2 where
-- exercicio 1

ex1A = [ 11^x | x <- [0..6] ]

ex1B = [ x | x <- [1..40], mod x 4 /= 0]

ex1C = [ 'A':x:"BB" | x <- ['a'..'g']]

ex1D = [ x | x <- [5,8 .. 41], x /= 14, x /= 23, x /= 34]

ex1E = [1/2^x | x <- [0..5]]

ex1F = [1,10 .. 64]

ex1G = [ x | x <- [2,4 .. 30], x /= 6, x /= 14, x /= 20, x /= 26]

ex1H = [ x:[] | x <- ['@'..'L'], x /= 'B', x /= 'F', x /= 'H', x /= 'I', x /= 'K']

ex2 x = even (length x)

ex3 x = [reverse y | y<-x] 

ex4 b = [length a | a<-b, mod(length a ) 2 /= 0 ]

ex5 x = last (reverse x)

ex6 :: String -> Bool
ex6 x = x == reverse x

ex7 x = (x, x*2, x*3, x*4)

--module Cap3 where



-- 3.1)
data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs xs = [pergNum x | x <- xs]

and' :: Pergunta -> Pergunta -> Int
and' Nao Nao = 0
and' Nao Sim = 0
and' Sim Nao = 0
and' Sim Sim = 1

or' :: Pergunta -> Pergunta -> Int
or' Nao Nao = 0
or' Nao Sim = 1
or' Sim Nao = 1
or' Sim Sim = 1

not' :: Pergunta -> Int
not' Nao  = 1
not' Sim  = 0

-- 3.2)

data Temperatura = Celsius | Farenheit | Kelvin deriving Show

converterCelsius :: Temperatura -> Double -> Double
converterCelsius Farenheit x = (x-32)/1.8
converterCelsius Kelvin x = x-273

converterKelvin :: Temperatura -> Double -> Double
converterKelvin Celsius x = x+273
converterKelvin Farenheit x = (x-32)/9

converterFarenheit :: Temperatura -> Double -> Double
converterFarenheit Celsius x = x+273
converterFarenheit Kelvin x = (x-273)/9

-- 3.3)
data Joken = Pedra | Papel | Tesoura deriving Show
data Result = Player1 | Player2 | Empate  deriving Show

jokenPo :: Joken -> Joken -> Result
jokenPo Papel Pedra = Player1
jokenPo Pedra Tesoura = Player1
jokenPo Tesoura Papel = Player1
jokenPo Pedra Papel = Player2
jokenPo Tesoura Pedra = Player2
jokenPo Papel Tesoura = Player2
jokenPo Papel Papel = Empate
jokenPo Pedra Pedra = Empate
jokenPo Tesoura Tesoura = Empate

-- 3.4)
ex1 :: String -> String
ex1 x = [ if y == 'a' || y == 'e' || y == 'i' || y == 'o' || y == 'u' then toUpper(y) else y |  y <- x]

ex1' :: String -> String
ex1' x = [ if elem y "aeiou" then toUpper(y) else y |  y <- x]

ex1'' :: String -> String
ex1'' x = [y | y <- x, elem y "aeiouAEIOU"] 


-- 3.5)
data Imp = Inch | Yard | Foot deriving Show

converterMetros :: Imp -> Double -> Double
converterMetros Inch x = x*0.0254
converterMetros Yard x = x*0.9144
converterMetros Foot x = x*0.3048

converterImperial :: Double -> Imp -> Double
converterImperial x Inch = x/0.0254
converterImperial x Yard = x/0.9144
converterImperial x Foot = x/0.3048

-- 3.6) Faça um novo tipo chamado Mes , que possui como valores todos os meses do ano. Implemente:
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Show,Enum)

-- A função checaFim , que retorna o número de dias que cada mês possui (considere fevereiro tendo 28 dias).
checaFim :: Mes -> Int
checaFim Fevereiro = 28
checaFim Abril = 30
checaFim Junho = 30
checaFim Setembro = 30
checaFim Novembro = 30
checaFim _ = 31

-- A função prox , que recebe um mês atual e retorna o próximo mês.
proximoMes :: Mes -> Mes
proximoMes Dezembro = Janeiro
proximoMes m = toEnum((fromEnum m) + 1) -- toEnum - Int -> a / fromEnum - a -> Int
-- succ - próximo
-- pred - anterior
-- proximoMes m = toEnum(succ(fromEnum m)) sucessor


--teste :: Int -> Mes
--teste x = toEnum(x)

-- A função estacao, que retorna a estação do ano de acordo com o mês e com o hemisfério. Use apenas tipos criados pela palavra data aqui.
data EstacaoDoAno = Primavera | Verao | Outono | Inverno deriving Show
data Hemisferio = Sul | Norte deriving Show

estacao :: Hemisferio -> Mes -> EstacaoDoAno
estacao  Sul Setembro = Primavera
estacao  Sul Outubro = Primavera
estacao  Sul Novembro = Primavera
estacao  Sul Dezembro = Verao
estacao  Sul Janeiro = Verao
estacao  Sul Fevereiro = Verao
estacao  Sul Marco = Outono
estacao  Sul Abril = Outono
estacao  Sul Maio = Outono
estacao  Sul Junho = Inverno
estacao  Sul Julho = Inverno
estacao  Sul Agosto = Inverno

estacao  Norte Setembro = Outono
estacao  Norte Outubro = Outono
estacao  Norte Novembro = Outono
estacao  Norte Dezembro = Inverno
estacao  Norte Janeiro = Inverno
estacao  Norte Fevereiro = Inverno
estacao  Norte Marco = Primavera
estacao  Norte Abril = Primavera
estacao  Norte Maio = Primavera
estacao  Norte Junho = Verao
estacao  Norte Julho = Verao
estacao  Norte Agosto = Verao

-- 3.7)
palindromoo :: String -> Bool
palindromoo x
           | (reverse x) == x = True
           | otherwise =  False
           
-- 3.8)
elimina :: [Int] -> [Int]
elimina x = [ y | y<-(reverse x), mod y 2 /= 0, mod y 7 /= 0, y > 0 ]

-- 3.9)
tupla :: String -> String -> String -> (String, String ,String)
tupla x y z = (reverse x, reverse y, reverse z)

-- 3.10) errado TENTAR USAR O take
--revNum :: String -> Int -> String
--revNum x y = [ if((elemIndex w x) < y)then reverse w else w | w<-x ] 

-- 3.11) 
data Bin = Zero | Um deriving Eq
data Funcao = Soma2 | Maior | Menor | Mult2

aplicar :: Funcao -> Bin -> Bin -> Bin
aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Zero Um = Um
aplicar Soma2 Um Um = Zero
aplicar Maior Zero Zero = Zero
aplicar Maior Um _ = Um
aplicar Maior _ Um = Um
aplicar Menor Um Um = Um
aplicar Menor _ Zero = Zero
aplicar Menor Zero _ = Zero
aplicar Mult2 Zero _ = Zero
aplicar Mult2 Um Um = Um
aplicar Mult2 Um Zero = Zero

-- 3.12)
binList :: [Bin] -> [Int]
binList x = [ if(y == Um)then 1 else 0 | y<-x]
-- binList x = [ conv y | y<-x]

conv :: Bin -> Int
conv Um = 1
conv Zero = 0

-- 3.13)
data Metros = Metros { dimensao :: Int, medida :: Double } | MetragemInvalida deriving Show

areaQuadrada :: Metros -> Metros
areaQuadrada (Metros _ 0) = MetragemInvalida
areaQuadrada x = Metros 2 ((medida x)*2)

areaTriangulo :: Metros -> Metros -> Metros
areaTriangulo (Metros _ 0) _ = MetragemInvalida
areaTriangulo x y = Metros 2 (((medida x)*(medida y))/2)

areaCubo :: Metros -> Metros
areaCubo x  = Metros 4 ((medida x)*(medida x)*(medida x))


-- 3.14)
data Teste = Nao' | Sim' { x :: String } deriving Show

isNomeValido :: String -> Teste
isNomeValido "" = Nao'
isNomeValido x = Sim' x

-- 3.15)

data Temperatura' =  Temperatura' { temp :: Temperatura, c :: Double } deriving Show

converterCelsius' :: Temperatura' -> Double
converterCelsius' (Temperatura' Farenheit x) = (x-32)/1.8
converterCelsius' (Temperatura' Kelvin x) = x-273


converterKelvin' :: Temperatura' -> Double
converterKelvin' (Temperatura' Celsius x) = x+273
converterKelvin' (Temperatura' Farenheit x) = (x-32)/9

converterFarenheit' :: Temperatura' -> Double
converterFarenheit' (Temperatura' Celsius x) = x+273
converterFarenheit' (Temperatura' Kelvin x) = (x-273)/9

-- 3.16)
data Numero = Ok { n :: Double} | Error { s :: String } deriving Show

dividir :: Double -> Double -> Numero
dividir _ 0 = Error "deu merda"
dividir x y = Ok (x/y)

-- 3.17)
data Cripto = Mensagem {txt :: String} | Cifrado {txt :: String} | Erro deriving Show

encriptar :: String -> Cripto
encriptar x = Cifrado (map succ x)

decriptar :: String -> Cripto
decriptar x = Mensagem (map pred x)

-- 3.18)

encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos x = [ testar y | y<-x ]

testar :: Cripto -> Cripto
testar (Mensagem y) = encriptar y
testar (Cifrado _) = Erro

-- 3.19)
data Cambio = Euroo | Dollarr | Reall deriving Show

data Moeda = Moeda {val :: Double, cur :: Cambio} deriving Show

converterMoeda :: Cambio -> Double
converterMoeda Reall = 1
converterMoeda Dollarr = 3.11
converterMoeda Euroo = 3.75

converterCambio :: Cambio -> Double -> Cambio -> Moeda
converterCambio x y z = Moeda (y*(converterMoeda x)/(converterMoeda z)) z

-- 3.20)

converteTodosReal :: [Moeda] -> [Moeda]
converteTodosReal x = map convR x

convR :: Moeda -> Moeda
convR (Moeda x Reall) = (Moeda x Reall)
convR (Moeda x Dollarr) = (Moeda (x*3.11) Reall)
convR (Moeda x Euroo) = (Moeda (x*3.75) Reall)

-- 3.21)
maxMoeda :: [Moeda] -> Double
maxMoeda x = maximum [(val y) | y<-x]



--module Cap4 where


-- 4.1)
media :: [Double] -> Double
media x = (/) (foldl (+) 0 x) (fromIntegral (length x)) 

-- 4.2)
palindromo :: [String] -> [String]
palindromo x = [ y | y<-x, y == reverse y]

-- 4.3)
filtraPar :: [Int] -> [Int]
filtraPar x = filter even x

filtraImpar :: [Int] -> [Int]
filtraImpar x = filter odd x

--4.4)
primo :: [Int] -> [Int]
primo [] = []
primo (x:xs)
           | length (ehPrimo x [2..x-1]) /= 0 = primo xs
           | otherwise = x: primo xs

ehPrimo :: Int -> [Int] -> [Int]
ehPrimo 1 _ = []
ehPrimo x y = filter (<=0) (map (\w -> mod x w) y)

-- 4.5)
list :: [Int] -> [Int]
list x = filter (\x -> mod x 4 /= 0) (map (*2) x)

-- 4.6)
func :: (String -> String) -> String -> String
func f s = (reverse s) ++ f "exec4.6 "

f :: String -> String
f x = reverse x

-- 4.7)

data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Eq)

dias :: [Dia] -> [Dia]
dias x = filter (\w -> w==Terca) x

-- 4.8)

data DimDim = Real | Dolar deriving (Show, Eq)
data Dinheiro = Dinheiro {valor :: Double, correncia :: DimDim} deriving Show

converter :: Dinheiro -> Dinheiro
converter (Dinheiro x Real) = Dinheiro (x*3.11) Dolar
converter (Dinheiro x Dolar) = Dinheiro (x/3.11) Real

filterDolar :: [Dinheiro] -> [Dinheiro]
filterDolar x = filter (\z -> (correncia z) == Dolar) x

somarDolar :: [Dinheiro] -> Double
somarDolar x = foldl (\c i -> c+i) 0 (map (\z -> (valor z)) (filter (\m -> (correncia m) == Dolar) x)) 

contarDolar :: [Dinheiro] -> Int
contarDolar x = length $ filter (\z -> (correncia z) == Dolar) x

-- 4.9)

contN :: [Int] -> Int
contN x = foldl (\cont _ -> cont+1) 0 (filter (<0) x)

contP :: String -> Int
contP x = foldl (\cont _ -> cont+1) 0 (filter (\l -> elem l "pP") x)

contD :: [Dia] -> Int
contD x = foldl (\cont _ -> cont+1) 0 (filter (\d -> d == Sabado) x)

contDias :: [Dia] -> Int
contDias x = foldl (+) 0 (map (\z -> convertDtoI z) x)

convertDtoI :: Dia -> Int
convertDtoI Segunda = 1
convertDtoI Terca = 2
convertDtoI Quarta = 3
convertDtoI Quinta = 4
convertDtoI Sexta = 5
convertDtoI Sabado = 6
convertDtoI Domingo = 7


