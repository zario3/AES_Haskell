module Cipher where

import Bytes

import Corpss

import Z_sur_nZ

import Polynomes

import Anneaux

----------------------------------------------Cipher fonctions-----------------------------------------------



--Transformation subBytes() sur un byte de AES
subBytes :: Byte -> Byte 
subBytes p = (modulo ((inv_modulo_mx (rem_zero p)) `multPoly` q) d) `addPoly` r
            where q = Poly [Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 1] 
                  r = Poly [Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 0]  
                  d = Poly [Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1] 

--Transformation subBytes() sur une colonne du state AES
subWord :: Polynome Byte -> Polynome Byte
subWord (Poly [a,b,c,d]) = Poly [subBytes a, subBytes b, subBytes c, subBytes d]

--Transformation subBytes() de AES
subBytes_state :: State -> State
subBytes_state [s0, s1, s2, s3] = [subWord s0,subWord s1,subWord s2,subWord s3]

--Transformation mixedColumn() sur une colonne de AES
mixed_column :: Polynome Byte -> Polynome Byte 
mixed_column (Poly [s0, s1, s2, s3]) = Poly [s0', s1', s2', s3']
  where
    s0' = (mult (hexa_bytes "02") s0) `add` (mult (hexa_bytes "03") s1) `add` s2 `add` s3
    s1' = (mult (hexa_bytes "02") s1) `add` (mult (hexa_bytes "03") s2) `add` s3 `add` s0
    s2' = (mult (hexa_bytes "02") s2) `add` (mult (hexa_bytes "03") s3) `add` s0 `add` s1
    s3' = (mult (hexa_bytes "02") s3) `add` (mult (hexa_bytes "03") s0) `add` s1 `add` s2

--Transformation mixedColumn() de AES   
mixed_columns:: State -> State
mixed_columns[s0, s1, s2, s3] = [mixed_column s0,mixed_column s1,mixed_column s2,mixed_column s3]

--Transformation shiftRows() de AES
shiftRows :: State -> State
shiftRows [s0, s1, s2, s3] = [s0', s1', s2', s3']
  where
    s0' = Poly [s0 `ind` 0 , s1 `ind` 1 , s2 `ind` 2 , s3 `ind` 3]
    s1' = Poly [s1 `ind` 0 , s2 `ind` 1 , s3 `ind` 2 , s0 `ind` 3]
    s2' = Poly [s2 `ind` 0 , s3 `ind` 1 , s0 `ind` 2 , s1 `ind` 3]
    s3' = Poly [s3 `ind` 0 , s0 `ind` 1 , s1 `ind` 2 , s2 `ind` 3]

shiftRows _ = []

--Sous fonction qui additionne deux bytes
addroundkey_Bytes :: Byte -> Byte -> Byte
addroundkey_Bytes p1 p2 = addPoly p1 p2

--Transformation addroundkey() sur colonne de AES 
addroundkey :: Polynome Byte -> Polynome Byte -> Polynome Byte
addroundkey (Poly [a0,a1,a2,a3]) (Poly [b0,b1,b2,b3]) = Poly [c0,c1,c2,c3] where 
                              c0 = a0 `addroundkey_Bytes` b0
                              c1 = a1 `addroundkey_Bytes` b1
                              c2 = a2 `addroundkey_Bytes` b2
                              c3 = a3 `addroundkey_Bytes` b3

--Transformation addroundkey() de AES
addroundkey_state:: State -> State -> State
addroundkey_state[s0, s1, s2, s3] [a0, a1, a2, a3] = [s0 `addroundkey` a0,s1 `addroundkey` a1,s2 `addroundkey` a2,s3 `addroundkey` a3]

rotWord :: Polynome Byte -> Polynome Byte           
rotWord (Poly [a0,a1,a2,a3]) = Poly [a1,a2,a3,a0]  

--Géneration des clefs pour différents round
key_expansion :: State -> Int -> State
key_expansion key k = aux key k where 
                    aux state 0 = state
                    aux state n = aux (key_round state (recon (k-n+1))) (n-1)                       

--Effectuer un round de key_expansion 
key_round :: State -> Polynome Byte -> State
key_round key recon_n = [colonne_1, colonne_2, colonne_3, colonne_4] where 
                        colonne_1 = (key!!0) `addroundkey` (subWord (rotWord(key!!3))) `addroundkey` recon_n
                        colonne_2 = (key!!1) `addroundkey` colonne_1
                        colonne_3 = (key!!2) `addroundkey` colonne_2
                        colonne_4 = (key!!3) `addroundkey` colonne_3
                        
--Fonction de Rcon qui renvoie le polynome byte qui correspond au round
recon :: Int -> Polynome Byte
recon k = Poly [aux k (hexa_bytes "01"), hexa_bytes "00",hexa_bytes "00", hexa_bytes "00"] where
  aux k p | k == 1 = p
          | otherwise = aux (k-1)  $ mult (hexa_bytes "02") p

--La clé utilisé dans cet algorithm
w0 = Poly [hexa_bytes "2B", hexa_bytes "7E",hexa_bytes "15", hexa_bytes "16"]
w1 = Poly [hexa_bytes "28", hexa_bytes "AE",hexa_bytes "D2", hexa_bytes "A6"]
w2 = Poly [hexa_bytes "AB", hexa_bytes "F7",hexa_bytes "15", hexa_bytes "88"]
w3 = Poly [hexa_bytes "09", hexa_bytes "CF",hexa_bytes "4F", hexa_bytes "3C"]
key_ex = [w0, w1, w2, w3]

--Un round du chiffrement AES
cipher_round :: State -> Int -> State
cipher_round state 0 = addroundkey_state state key_ex
cipher_round state 10 = addroundkey_state  (shiftRows (subBytes_state state)) (key_expansion key_ex 10)
cipher_round state k = addroundkey_state (mixed_columns (shiftRows (subBytes_state state))) (key_expansion key_ex k)

--L'algorithme du chiffrement AES
cipher :: State -> State
cipher state = aux state 0 where
  aux s 11 = s 
  aux s n = aux (cipher_round s n) (n+1)
  

--------------------------------------------inverse cipher fonctions-------------------------------------------

--Transformation inverse de shiftRows()
invShiftRows :: State -> State
invShiftRows [s0, s1, s2, s3] = [s0', s1', s2', s3']
  where
    s0' = Poly [s0 `ind` 0 , s3 `ind` 1 , s2 `ind` 2 , s1 `ind` 3]
    s1' = Poly [s1 `ind` 0 , s0 `ind` 1 , s3 `ind` 2 , s2 `ind` 3]
    s2' = Poly [s2 `ind` 0 , s1 `ind` 1 , s0 `ind` 2 , s3 `ind` 3]
    s3' = Poly [s3 `ind` 0 , s2 `ind` 1 , s1 `ind` 2 , s0 `ind` 3]

invShiftRows _ = []

--Transformation inverse de mixedColumn()
inv_mixed_columns:: State -> State
inv_mixed_columns [s0, s1, s2, s3] = [inv_mixed_column s0,inv_mixed_column s1,inv_mixed_column s2,inv_mixed_column s3]

--Transformation inverse de mixedColumn() sur une colonne
inv_mixed_column :: Polynome Byte -> Polynome Byte 
inv_mixed_column (Poly [s0, s1, s2, s3]) = Poly [s0', s1', s2', s3']
  where
    s0' = (mult (hexa_bytes "0E") s0) `add` (mult (hexa_bytes "0B") s1) `add` (mult (hexa_bytes "0D") s2) `add` (mult (hexa_bytes "09") s3)
    s1' = (mult (hexa_bytes "09") s0) `add` (mult (hexa_bytes "0E") s1) `add` (mult (hexa_bytes "0B") s2) `add` (mult (hexa_bytes "0D") s3)
    s2' = (mult (hexa_bytes "0D") s0) `add` (mult (hexa_bytes "09") s1) `add` (mult (hexa_bytes "0E") s2) `add` (mult (hexa_bytes "0B") s3)
    s3' = (mult (hexa_bytes "0B") s0) `add` (mult (hexa_bytes "0D") s1) `add` (mult (hexa_bytes "09") s2) `add` (mult (hexa_bytes "0E") s3)

--Transformation inverse de subBytes() sur un byte
invsubBytes :: Byte -> Byte 
invsubBytes p = if bytes_hexa p == "00" then hexa_bytes "52"
                else if bytes_hexa p =="01" then hexa_bytes "09" 
                else inv_mult $ modulo ((p `multPoly` q) `addPoly` r ) d
                     where q = Poly [Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 0] 
                           r = Poly [Z2Z 1,Z2Z 0,Z2Z 1,Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 0] 
                           d = Poly [Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1] 


--Transformation inverse de subBytes() sur une colonne
invsubWord :: Polynome Byte -> Polynome Byte
invsubWord (Poly [a,b,c,d]) = Poly [invsubBytes a, invsubBytes b, invsubBytes c, invsubBytes d]

--Transformation inverse de subBytes() 
invsubBytes_state :: State -> State
invsubBytes_state [s0, s1, s2, s3] = [invsubWord s0,invsubWord s1,invsubWord s2,invsubWord s3]

--Un round du chiffrement inverse de AES
inv_cipher_round :: State -> Int -> State
inv_cipher_round state 0 = invsubBytes_state $ invShiftRows $ addroundkey_state state (key_expansion key_ex 10)
inv_cipher_round state 10 = addroundkey_state state (key_expansion key_ex 0)
inv_cipher_round state k = invsubBytes_state $ invShiftRows $ inv_mixed_columns $ addroundkey_state state (key_expansion key_ex (10 - k))

--L'algorithme du chiffrement inverse de AES
inv_cipher :: State -> State
inv_cipher state = aux state 0 where
  aux s 11 = s 
  aux s n = aux (inv_cipher_round s n) (n+1)