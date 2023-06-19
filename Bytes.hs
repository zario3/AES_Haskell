{-# LANGUAGE FlexibleInstances #-}


module Bytes where


import Corpss

import Z_sur_nZ

import Polynomes

import Anneaux

--Création du type Byte représantant un pollynome de Z/2Z de degré 7
type Byte = Polynome Z_sur_2Z 

--Création du type state étant une liste de polynomes de Bytes, c'est le type du input et output de AES
type State = [Polynome Byte]


-- Polynome m(x) = x8 + x4 + x3 + x + 1
m_x = Poly [Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1]

--Calcule le modulo d'un Byte par le polynome m(x)
modulo_mx :: Byte -> Byte 
modulo_mx p = modulo p m_x

--Calcul de l'inverse d'un Byte tout en restant de degré inférieur à 8
inv_modulo_mx :: Byte -> Byte 
inv_modulo_mx p | modulo_mx p == Poly [] = Poly []
                | otherwise = b       where (_,b,_) = euclide (rem_zero p) m_x


--Instantiation du type Byte dans la classe Corps.
instance Corps Byte where
  n_add = Poly []
  est_nul = est_byte_nul
  add = addPoly
  inv_add = oppPoly
  n_mult = Poly [Z2Z 1]
  mult p q = modulo_mx (multPoly p q)
  inv_mult = inv_modulo_mx


--Polynome a_X
a0 = hexa_bytes "02"
a1 = hexa_bytes "01"
a2 = hexa_bytes "01"
a3 = hexa_bytes "03"
a_x = Poly [a0, a1, a2, a3]

pol_red :: Polynome Byte 
pol_red = Poly [Poly [Z2Z 1], Poly [Z2Z 0], Poly [Z2Z 0], Poly [Z2Z 1]]


--Renvoi le coefficient à l'indice n d'un polynome 
ind :: Polynome Byte -> Int -> Byte 
ind (Poly xs) n = xs!!n 

--Tester si un byte est nul
est_byte_nul :: Byte -> Bool
est_byte_nul p =  
  if ((rem_zero p) == (Poly [])) then
    True  
  else False 

-------------------------------------------Conversions et affichage-----------------------------------------------------------

hexa_bytes :: String -> Byte
hexa_bytes (x:xs) = Poly (aux (x:xs)) where 
  aux [] = []
  aux "0" = [Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0]
  aux "1" = [Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0]
  aux "2" = [Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 0]
  aux "3" = [Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 0]
  aux "4" = [Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 0]
  aux "5" = [Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 0]
  aux "6" = [Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 0]
  aux "7" = [Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 0]
  aux "8" = [Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1]
  aux "9" = [Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 1]
  aux "A" = [Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 1]
  aux "B" = [Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 1]
  aux "C" = [Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 1]
  aux "D" = [Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 1]
  aux "E" = [Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 1]
  aux "F" = [Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 1]
  aux ys = (aux [last ys]) ++ (aux (init ys))


bytes_hexa :: Byte -> String
bytes_hexa (Poly pol) = reverse (aux pol) where
  aux [] = ""
  aux ([Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0]) = "0"
  aux ([Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0]) = "1"
  aux ([Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 0]) = "2"
  aux ([Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 0]) = "3"
  aux ([Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 0]) = "4"
  aux ([Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 0]) = "5"
  aux ([Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 0]) = "6"
  aux ([Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 0]) = "7"
  aux ([Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1]) = "8"
  aux ([Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 1]) = "9"
  aux ([Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 1]) = "A"
  aux ([Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 1]) = "B"
  aux ([Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 1]) = "C"
  aux ([Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 1]) = "D"
  aux ([Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 1]) = "E"
  aux ([Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 1]) = "F"
  aux (a:b:c:d:xs) = (aux [a,b,c,d]) ++ (aux xs)
  aux [a,b,c] = aux [a,b,c, Z2Z 0]
  aux [a,b] = aux [a, b, Z2Z 0, Z2Z 0]
  aux [a] = aux [a, Z2Z 0, Z2Z 0, Z2Z 0]

printState :: State -> IO ()
printState [s0, s1, s2, s3] = do 
  putStrLn ((aux s0 0) ++ " " ++ (aux s1 0) ++ " " ++ (aux s2 0) ++ " " ++ (aux s3 0))
  putStrLn ((aux s0 1) ++ " " ++ (aux s1 1) ++ " " ++ (aux s2 1) ++ " " ++ (aux s3 1))
  putStrLn ((aux s0 2) ++ " " ++ (aux s1 2) ++ " " ++ (aux s2 2) ++ " " ++ (aux s3 2))
  putStrLn ((aux s0 3) ++ " " ++ (aux s1 3) ++ " " ++ (aux s2 3) ++ " " ++ (aux s3 3))
  where aux byte n | n <= (degre $rem_zero byte) = bytes_hexa (byte `ind` n)
                   | otherwise = "00"



-------------------------------------------------------------------------------------------------------------------------------------------

 

