module Conjunto (
      vacio                 -- Crea un conjunto vacío
    , aniade                -- Añade un elemento al principio del conjunto
    , inserta               -- Inserta un elemento en la posición deseada en el conjunto
    , borra                 -- Borra el elemento de la posición deseada del conjunto
    , tamanio               -- Número de elementos del conjunto
    , pertenece             -- Dice si un elemento pertenece al conjunto
    , subconjuntoDe         -- Dice si un conjunto es un subconjunto de otro
    , unionDe               -- Operador de unión de conjuntos 
    , diferenciaDe          -- Operador de diferencia de conjuntos
    , diferenciaSimetricaDe -- Operador de diferencia simétrica de conjuntos
    , interseccionDe        -- Operador de intersección de conjuntos
    , minDe                 -- Mínimo del conjunto
    , maxDe                 -- Máximo del conjunto
    , fromArray             -- Crea un conjunto desde un array
    , toArray               -- Pasa el conjunto a un array
  ) where

import Data.List

-- Conjunto como listas 
data Conjunto a = Conj [a]

-- Instanciación de Show
instance (Show a) => Show (Conjunto a) where
  showsPrec _ (Conj s) c = showConjunto s c


showConjunto [] cad = showString "{}" cad
showConjunto (x:xs) cad = showChar '{' (shows x (showlista xs cad))
  where
    showlista [] cad = showChar '}' cad
    showlista (x:xs) cad = showChar ',' (shows x (showlista xs cad))


-- Instanciación de Foldable
instance Foldable Conjunto where
  foldMap f (Conj []) = mempty
  foldMap f (Conj (x:xs)) = f x `mappend` foldMap f xs


--  Vacio
vacio :: Conjunto a
vacio = Conj []

-- Añade
aniade :: Eq a => a -> Conjunto a -> Conjunto a
aniade x (Conj a) = Conj (x:a)

-- Inserta
inserta :: Eq a => a -> Int -> Conjunto a-> Conjunto a
inserta x pos (Conj a)
  | pos > (length a) = inserta x (length a) (Conj a)
  | pos > 0 && pos <= length a = Conj (prim ++ (x:seg)) 
  | otherwise = aniade x (Conj a)
  where 
    (prim,seg) = splitAt pos a


-- Borra
borra :: Int -> Conjunto a -> Conjunto a
borra pos (Conj a)
  | pos > length a = borra (length a) (Conj a)
  | pos > 0 && pos <= length a = Conj (prim ++ (drop 1 seg))
  | otherwise = borra 0 (Conj a)
  where 
    (prim,seg) = splitAt pos a

-- Tamaño
tamanio :: Conjunto a -> Int
tamanio (Conj a) = length a

-- Pertenece
pertenece :: Eq a => a -> Conjunto a -> Bool
pertenece x (Conj a) = elem x a

-- Subconjunto de 
subconjuntoDe :: Eq a => Conjunto a -> Conjunto a -> Bool
subconjuntoDe (Conj a) (Conj b) = isSubsequenceOf a b

-- Unión
unionDe :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
unionDe (Conj a) (Conj b) = Conj (a `union` b)

-- Intersección
interseccionDe :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
interseccionDe (Conj a) (Conj b) = Conj (a `intersect` b)

-- Diferencia
diferenciaDe :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
diferenciaDe (Conj a) (Conj b) = Conj (a \\ b)

-- Diferencia Simétrica
diferenciaSimetricaDe :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
diferenciaSimetricaDe (Conj a) (Conj b) = Conj (dif)
  where
    dif = ([x | x <- (a `union` b), (elem x (a `union` b)) && not (elem x (a `intersect` b))])

-- Mínimo del conjunto
minDe :: Ord a =>Conjunto a -> a
minDe (Conj a) = minimum a

-- Máximo del conjunto 
maxDe :: Ord a => Conjunto a -> a
maxDe (Conj a) = maximum a

-- From Array
fromArray :: Eq a => [a] -> Conjunto a
fromArray lis = foldr aniade vacio lis

-- To Array
toArray :: Num a => Conjunto a -> [a]
toArray (Conj a) = a
