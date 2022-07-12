--  EJERCICIO 1 --


-- Algoritmo extendido de Euclides - Versión Recursiva --

rxeuc :: Integer -> Integer -> [Integer]
rxeuc m 0 = [1, 1, 0]
rxeuc m n = [gcd m n, u, v]
    where (u, v) = rbezout m n

rbezout:: Integer -> Integer -> (Integer, Integer)
rbezout m 0 = (1, 0)
rbezout m n = (u, v - cociente * u)
    where (cociente, resto) = (m `div` n, m `mod` n)
          (v, u) = rbezout n resto


-- Algoritmo extendido de Euclides - Versión Iterativa --


ixeuc :: Integer -> Integer -> [Integer]
ixeuc m 0 = [1, 1, 0]
ixeuc m n = [gcd m n, res!!2, res!!4]where
    res = head (filter (\x -> (x!!1) == 0) (iterate ibezout [m,n,1,0,0,1]))

ibezout :: [Integer]-> [Integer]
ibezout [a0, a1, s0, s1, t0, t1] = [a0t, a1t, s0t, s1t, t0t, t1t]
    where
        q = a0 `div` a1
        a0t = a1
        a1t = a0 `mod` a1
        s0t = s1
        s1t = s0 - (s1*q)
        t0t = t1
        t1t = t0 - (t1*q)

