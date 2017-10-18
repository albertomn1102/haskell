module Aula7 where
-- type parameter
data Carteira a = Nada 
                |UmItem a 
                |DoisItens a a deriving Show
                
                
mostrarPrimeira :: Carteira a -> a

mostrarPrimeira (UmItem x) = x

mostrarPrimeira (DoisItens x _) = x

mostrar :: Show a => Carteira a -> String
mostrar Nada ="Vazio..."
mostrar (UmItem x)= "Elemento: "++ show x
mostrar (DoisItens x y)= "Elemento: "++ show x
                        ++"    Elemento2: " ++ show y


data Moeda = Euro | Real |Dollar 

--se quisermos fazer uma implementacao de show diferente da proposta pela linguagem tira a moeda
{-
class Show a where
    show :: a -> String
-}

instance Show Moeda where
    show Euro = "dinheiro"
    show Real = "merda"
    show Dollar = "verde"
    
instance Eq Moeda where
    Euro == Dollar = True
    Dollar == Euro = True
    Real == Real = True
    Dollar == Dollar = True
    Euro == Euro=True
    _==_ = False
    
instance (Carteira a) where
    Nada == Nada = True
    (UmItem x) == ()
    
    saida :: Int -> Int -> Bool
    saida a b = a<b