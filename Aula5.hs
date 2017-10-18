module Aula5 where

somar :: Int -> Int -> Int -> Int
somar x y z = x+y+z


--map joga uma funcao em uma lista

--filter : filtra os elementos de uma lista de acordo com um predicado (a -> Bool)

tamanho :: String -> Int
tamanho = length
ehprimo :: Int -> Bool
ehprimo n = filter (\x -> mod n x == 0) [1 .. n-1] == [1]

fat :: Int -> Int

fat n = foldl (*) 1 [1..n]

-- [1*1*2*3*4*5*6*7]

--operador ou funcao infixa
infixl 0 |>
(|>) :: Double -> Double
