module Prova where
import Data.Monoid

data Position = Clowns | Jokers | Middle

class (Music a) where 
    whereIn :: a -> Position
    
instance Music Int where
    whereIn a 
            | a>0 = Clowns
            | a==0 = Middle
            | otherwise = Jokers
    
instance (Ord a, Monoid a) => Monoid (Music a) where
    whereIn a 
            |a>mempty = Clowns
            |a==mempty = Middle
            |otherwise = Jokers