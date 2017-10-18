module Aula3 where

-- double dobro(double x);

dobro :: Double -> Double
dobro x = 2*x

somar :: Int -> Int -> Int -> Int --somar :: int x , int y, int z = int(x+y+z)(resultado)
somar x y z = x+y+z

f:: String -> Int

f ls = 1 + length ls

maiorQue :: Int -> Int -> Bool
maiorQue x n = x > n

--tipo enum so funciona para tipos enumeravel

data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Enum,Eq,Show)

--Pattern Matching: desconstroi um tipo fazendo com que seja revelada sua estrutura interna. No nosso caso, foi revelado os VALUE CONSTRUCTORS

agenda :: Dia -> String
agenda Segunda = "Dia de Cinema"
agenda Quarta = "Dia de futebol"
agenda Quinta = "Dia da praca e nossa"
agenda Sexta = "Dia de maldade"
agenda Sabado = "Dia de balada 10/10 topster"
agenda _ = "Dia de nao fazer nada"

converte :: Dia -> Int

converte Segunda = 1
converte Terca = 2
converte Quarta = 3
converte Quinta = 4
converte Sexta = 5
converte Sabado = 6
converte _ = 7

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Enum,Eq,Show)

traduzirPI :: Dia -> Day
traduzirPI Domingo = Sunday
traduzirPI Segunda = Monday
traduzirPI Terca = Tuesday
traduzirPI Quarta = Wednesday
traduzirPI Quinta = Thursday
traduzirPI Sexta = Friday
traduzirPI Sabado = Saturday

data Curso = ADS | SI | GE | GP | Log deriving Show

data Aluno = Aluno String Int Curso deriving Show

aniversario :: Aluno -> Aluno

aniversario (Aluno nome idade curso) = Aluno nome (idade+1) curso

data Naipe = Ouros | Espadas | Copas | Paus deriving (Enum,Show)

data Valor = As | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei deriving (Enum,Show)

data Carta = Carta Valor Naipe deriving Show

mistura = [(naipe,valores)|naipe <- [Ouros,Espadas,Copas,Paus],valores<-[As,Dois,Tres,Quatro,Cinco,Seis,Sete,Oito,Nove,Dez,Valete,Dama,Rei]]