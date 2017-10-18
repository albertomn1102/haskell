{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- FlexibleInstances
module Aula8 where

import Data.Monoid

data And = And Bool deriving Show

instance Monoid And where
    mempty = And True
    --mappend (And x) (And y) = And (x &&y)
    mappend (And True) (And True) = And True
    mappend _ _ = And False

-- Restricao de kind * ->
class SimNao a where
    simnao :: a -> Bool

-- 0 deu merda, nao ha como garantir que Num a possua zero    
instance (Ord a, Monoid a) => SimNao a where
    simnao x
        | x > mempty = True
        | otherwise = False
        
instance SimNao [a] where
    simnao [] = False
    simnao _ = True
    
instance SimNao Bool where
    simnao = id
