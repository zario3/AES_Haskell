module Polynomes where

import Corpss 
import Anneaux
import Z_sur_nZ

--Création du type Polynome dont les coefficients sont dans un Corps
newtype Polynome a = Poly [a] deriving (Eq, Show)

--Calcul du degre d'un polynome
degre :: (Corps a )=> Polynome a -> Int 
degre (Poly p) = (length p ) -1


--Addition de polynomes 
addPoly :: (Corps a) => Polynome a -> Polynome a -> Polynome a
addPoly (Poly p1) (Poly p2) = Poly $aux p1 p2
    where
        aux [] [] = []
        aux (x:xs) [] = x : aux xs []
        aux [] (y:ys) = y : aux [] ys
        aux (x:xs) (y:ys) = (add x y) : aux xs ys

--Multiplication d'un polynome par un scalaire du corps 
mult_par_scalaire :: (Corps a) => a -> Polynome a ->  Polynome a 
mult_par_scalaire s (Poly p) = Poly (map (mult s) p)

--Calcul de l'inverse par l'addition d'un polynome
oppPoly :: (Corps a) => Polynome a -> Polynome a 
oppPoly (Poly p) = Poly (map (inv_add) p)

--Multiplication par le polynome x puissance n
mult_par_xn :: (Corps a)=> Int -> Polynome a -> Polynome a 
mult_par_xn _ (Poly []) = Poly []
mult_par_xn n (Poly p) = Poly ((replicate n n_add)++p)

--Multiplication de deux polynomes
multPoly :: (Corps a) => Polynome a -> Polynome a -> Polynome a
multPoly (Poly []) _ = Poly []
multPoly _ (Poly []) = Poly []
multPoly (Poly (c1:cs1)) (Poly p2) = addPoly (mult_par_scalaire c1 (Poly p2)) (mult_par_xn 1 (multPoly (Poly cs1)  (Poly p2)))



--On ajoute le type "polynomes à coefficients dans un corps" dans la classe Anneau 
instance Corps a => Anneau (Polynome a) where
  neutre_add = Poly []
  addition = addPoly
  inverse = oppPoly
  neutre_mult = Poly [n_mult]
  multiplication = multPoly

--Donne le coefficient dominant d'un polynome
leading :: (Corps a) => Polynome a -> a
leading (Poly []) = n_add
leading (Poly (c:cs))
    | null cs = c
    | otherwise = leading (Poly cs)

--Division euclidienne de deux polynomes , renvoi le couple (quotient, reste)
division :: (Eq a, Corps a) => Polynome a -> Polynome a -> (Polynome a, Polynome a)
division a b = go (Poly []) a
  where go q r
          | degre r < degre b = (q, r)
          | otherwise =
              let d = degre r - degre b
                  c = mult (leading r)  (inv_mult (leading b))
                  t = mult_par_xn d (Poly [c])
              in go (q `addPoly` t)  (rem_zero (r `addPoly` (inverse (t `multPoly` b))))


--Reverse de la liste des coeffs du polynome 
reversePoly :: Polynome a -> Polynome a
reversePoly (Poly p) = Poly (reverse p)


--Enleve les zero de la fin des polynomes pour ne pas fausser le calcul du degre qui tient en compte la longueur de la liste
rem_zero :: (Eq a, Corps a) => Polynome a -> Polynome a 
rem_zero (Poly []) = Poly []
rem_zero (Poly p) | not (est_nul (last p)) = Poly p
                  | otherwise = rem_zero (Poly (init p))

--Calcule le modulo d'un polynme par un autre
modulo :: (Eq a, Corps a) => Polynome a -> Polynome a -> Polynome a 
modulo p q = snd (division p q)

--Fait l'algorithme d'euclide : Il renvoi un triplet (PGCD, 1er coeff, 2eme coeff)
euclide :: (Eq a, Corps a) => Polynome a -> Polynome a -> (Polynome a, Polynome a, Polynome a)
euclide a b | a == neutre_add = (b, neutre_add, neutre_mult)
            | otherwise = (gcd, y `addPoly` oppPoly((fst (division b a)) `multPoly` x), x)
            where (gcd, x, y) = euclide (modulo b a) a
