module Z_sur_nZ where

import Anneaux
import Corpss

---------------------------------------------
-- Fonctions génériques de calcul modulo p --
---------------------------------------------

-- addition modulo p
addModP :: Integer -> Integer -> Integer -> Integer
addModP p n m = (n+m) `mod` p 

-- opposé modulo p
oppose :: Integer -> Integer -> Integer
oppose p n | n == 0    = 0
           | otherwise = p - n

-----------------------------------------------------------------
-- Définition de Z sur 2Z, instanciation dans la classe Anneau --
-----------------------------------------------------------------

--Création du type Z/2Z représentant des chiffres binaires
newtype Z_sur_2Z = Z2Z Integer    deriving (Show, Eq)

--Fonctions nécessaires à l'instanciation du nouveau type à la classe corps
addMod2 (Z2Z a) (Z2Z b) = Z2Z (addModP 2 a b)
oppose2 (Z2Z a) = Z2Z (oppose 2 a)
multMod2 (Z2Z a) (Z2Z b) = Z2Z ((a * b) `mod` 2  )


--Ajout de Z_sur_2Z a la classe Corps pour pouvoir les utiliser comme coefficients de polynomes
instance Corps Z_sur_2Z where
  n_add = Z2Z 0
  est_nul = ((Z2Z 0) ==)
  add = addMod2
  inv_add = oppose2
  n_mult = Z2Z 1
  mult = multMod2
  inv_mult (Z2Z a)| a==0 = Z2Z 0
                | otherwise = Z2Z 1