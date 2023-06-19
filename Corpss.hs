module Corpss where

--Création de la classe représentant la structure algébrique corps
class Corps a where
  n_add :: a
  est_nul :: a -> Bool
  add :: a -> a -> a  
  inv_add :: a -> a 
  n_mult :: a 
  mult :: a -> a -> a 
  inv_mult :: a -> a 


--Création du type réel
newtype Reel = R Float    deriving (Show, Eq)

--Instantiation des réels dans la classe corps
instance Corps Reel where
  n_add = R 0
  est_nul = ((R 0) ==)
  add (R a) (R b) = R (a+b)
  inv_add (R a) = R (-a)
  n_mult = R 1
  mult (R a) (R b) = R (a*b)
  inv_mult (R a)| a==0 = n_add
                | otherwise = R (1/a)

--Cette instantiation nous a permis de tester la classe corps

--Instantiation des réels dans Num
instance Num Reel where
  (R x) + (R y) = R (x + y)
  (R x) - (R y) = R (x - y)
  (R x) * (R y) = R (x * y)
  abs (R x) = R (abs x)
  signum (R x) = R (signum x)
  fromInteger x = R (fromInteger x)


