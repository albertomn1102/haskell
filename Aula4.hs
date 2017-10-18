module Aula4 where


data Correncia = Euro | Dollar | Real  deriving Show

data Dinheiro = Dinheiro { valor::Double, curr:: Correncia} deriving Show
     --           | Moeda Double Correncia 
     --           | Nada deriving Show
     
     
converterReal :: Dinheiro -> Dinheiro
converterReal (Dinheiro x Dollar) = Dinheiro (3.14*x) Real
converterReal (Dinheiro x Euro) = Dinheiro (3.71*x) Real
converterReal x = x

dobraDinheiro (Dinheiro x y)= Dinheiro (x*2) y

somarDinheiroReal:: Dinheiro -> Dinheiro -> Dinheiro

somarDinheiroReal din0 din1 = Dinheiro (valor(converterReal din0)+ valor(converterReal din1)) Real



