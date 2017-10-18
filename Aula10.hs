module Aula10 where

-- Qual o kind? * -> *
-- Eh polimorfico? Sim.
-- Quantos VC? 1
-- Quantos campos tem o VC? 1
-- Qual o tipo do campo? a
data Id a = Id a deriving Show

instance Functor Id where
    fmap ab (Id a) = Id (ab a)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- Polimorfica? Sim, recebe um parâmetro variável
-- [a] == [] a
-- f :: Id a -> [a] 
-- TRANSFORMACAO NATURAL: EH UMA FUNCAO QUE "TROCA"
-- FUNTOR E NAO POSSUI RESTRICAO ALGUMA.
-- f :: Id -> []
-- TEM A FORMA GENERICA: f :: F a -> G a 
-- f :: (Functor F, Functor G) => F a -> G a
-- ONDE F eh uma instancia de Functor e G tb.

f :: Id a -> [] a 
f (Id x) = [x]

f' :: a -> [] a
f' x = [x]

-- F = Id, G = []
g :: a -> [] a 
g x = [x]

-- F = [], G = Id
h :: [] a -> Id a 
h xs = Id (head xs)

h2 :: [] a -> Maybe a
h2 [] = Nothing
h2 xs = Just (head xs)


---------------------MÔNADAS------------------------------
-- uma modada (M) é um functor de kind 2 e duas transformações naturais

-- Def: uma mônada sobre uma categoria C (Hask) é a tripla (m, return, join), onde

-- > m é um tipo de kind * -> * instância de Functor
-- > return é uma transformação natural de tipo : return :: a -> m a (Troca Id por m) // nada por m
-- > join é uma transformação natural de tipo : join :: m(m a) -> m a

-- definição arrogante:
-- Uma Mônada da categoria C é apenas um monóide na Categoria dos (endo)Functores