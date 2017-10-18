 

data Status = Magro | Gordo | Saudavel deriving Show

fat:: Int -> Int
fat 0 = 1
fat n
    | n<=1=1
    | otherwise = n*fat(n-1)
    
modulo :: Double -> Double

modulo x
       |x > 0 = x
       |otherwise = -x
       
       
elimvogal :: String -> String

elimvogal [] = []
elimvogal (x:xs)
    |elem x "AEIOUaeiou" = elimvogal xs
    |otherwise = x : elimvogal xs
    
imc :: Double -> Double -> Status
imc peso altura
    | peso/(altura*altura) <= 18 = Magro
    | peso/(altura*altura) <= 25 = Saudavel
    | otherwise = Gordo
    where
        z = peso/(altura*altura)

data Metros a = Metros Double  deriving Show
data Um
data Dois
--Ghost Typping
areaQuadrado :: Metros Um -> Metros Dois
areaQuadrado (Metros m) = Metros (m*m)

--No programa rodando:
--Aula6> let m = Metros 10 :: Metros Um
--Aula6> :t m
--m :: Metros Um
--Aula6> areaQuadrado m
--Metros 100.0
--Aula6> :t (areaQuadrado m)
--(areaQuadrado m) :: Metros Dois




data Conjunto c = Conjunto [String] deriving Show
data X
data Y
exibe :: Conjunto X -> Conjunto Y
exibe (Conjunto p) = Conjunto(filter (\a ->reverse a == a)p)--palindromo

exibe2 :: Conjunto X -> Conjunto Y
exibe2 (Conjunto p) = Conjunto(filter (\a ->reverse a == a)p)--palindromo