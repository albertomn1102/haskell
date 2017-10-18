{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- FlexibleInstances
module Aula08 where
import Data.Monoid

--______________________________Monoid_____________________________________________
-- Data.monoid
-- def: um tipo "m" com uma operação binaria (<>) satisfazendo:
-- a) elemento neutro e (mempty):: m tal que:
-- a <> e = a
-- e <> a = a

-- b)associatividade: qualquer a,b,c :: m
-- (a <> b) <> c = a <> (a <> c)


-- mappend = (<>) :: m -> m -> m 
-- axioma, tem como verdade e nao precisa provar



data Carteira a = Nada | UmItem a deriving Show



-- functor
-- ab é sempre um funcao q troca a por binaria
-- fmap :: (a -> b) -> Carteira a -> Carteira b
instance Functor Carteira where
    fmap _ Nada = Nada
    fmap ab (UmItem a) = UmItem (ab a)
    fmap ab (DoisItens a1 a2) = DoisItens (ab a1) (ab a2)

-- O functor Maybe
-- data Maybe a = Nothing | Just a
-- instance Functor Maybe where
-- fmap ab Nothing = Nothing
-- fmap ab (Just a) = Just (ab a)

-- Maybe conserta funcoes parcial (head)
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)


