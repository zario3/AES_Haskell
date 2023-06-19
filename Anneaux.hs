module Anneaux where

--Création de la classe Anneaux
class Anneau a where
    neutre_add :: a 
    addition :: a -> a -> a
    inverse :: a -> a 
    neutre_mult :: a 
    multiplication :: a -> a -> a


--Anneau des entiers relatifs

instance Anneau Int where
    neutre_add = 0
    addition a b = a + b
    inverse a = -a
    neutre_mult = 1
    multiplication a b = a * b

