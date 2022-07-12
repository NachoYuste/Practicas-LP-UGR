-- Ejercicio 2 --

import Data.Bits

-- ALGORITMO DE STEIN - HASKELL --

bxeuc :: Int -> Int -> [Int]
bxeuc a b
    | a==b || b == 0 = [a,1,0]
    | a==0 = [b,0,1]
    | otherwise = [(res!!0)*(res!!1), res!!3, res!!4]
        where res = bucle1 1 a b

bucle1 :: Int -> Int -> Int -> [Int]
bucle1 p a b 
    | (mod a 2 /= 0) ||  (mod b 2 /= 0) = bucle2 [p,a,b,1,0,0,1,a,b]
    | otherwise = bucle1 (p `shiftL` 1) (a `shiftR` 1) (b `shiftR` 1)  

bucle2 :: [Int] -> [Int]
bucle2 [p,m,n,u,v,s,t,a,b]
    | (mod m 2 /= 0) = bucle3 [p,m,n,u,v,s,t,a,b]
    | (mod u 2 == 0) && (mod v 2 == 0) = bucle2 [p, m `shiftR` 1, n, u `shiftR` 1, v `shiftR` 1, s, t, a, b]
    | otherwise = bucle2 [p, m `shiftR` 1, n, (u+b) `shiftR` 1, (v-a) `shiftR` 1, s, t, a, b]

bucle3 :: [Int] -> [Int]
bucle3 [p,m,n,u,v,s,t,a,b]
    | (m == n) = [p,m,n,u,v,s,t,a,b]
    | (mod n 2 == 0) = resAux
    | (n < m) = bucle3 [p,n,m,s,t,u,v,a,b]
    | otherwise = bucle3 [p,m,n-m,u,v,s-u,t-v,a,b]
    where
        resAux
            | (mod s 2 == 0)&&(mod t 2 == 0) = bucle3 [p,m,n `shiftR` 1,u,v,s `shiftR` 1,t `shiftR` 1,a,b]  
            | otherwise =                      bucle3 [p,m,n `shiftR` 1,u,v,(s+b) `shiftR` 1,(t-a) `shiftR` 1,a,b]


{-

ALGORITMO DE STEIN - PYTHON

Implementación del algoritmo en Python sobre el que 
me he basado para la implementación en Haskell

def xbgcd (a,b):

    if a == b or b == 0: return a, 1, 0
    if a == 0: return b, 0, 1

    p=1

    while ~a&1 and ~b&1: # ambos a y b son pares
        a,b,p = a>>1, b>>1, p<<1

    m,n,u,v,s,t = a,b,1,0,0,1

    while ~m&1:
        m>>=1

        if ~u&1 and ~v&1:
            u,v=u>>1,v>>1
        else:
            u,v=(u+b)>>1,(v-a)>>1

    while m!=n:
    
        if ~n&1:
            n = n >> 1
    
            if ~s&1 and ~t&1:
                s,t = s>>1, t>>1
            else:
                s,t = (s+b)>>1, (t-a)>> 1

        elif n<m:
            m,n,u,v,s,t = n,m,s,t,u,v
        else:
            n,s,t = n-m, s-u, t-u
    
    return p*m,u,v

-}