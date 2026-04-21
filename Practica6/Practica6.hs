module Practica6 where

data Arbol a = Vacio | AB a (Arbol a) (Arbol a)
    deriving (Eq, Ord, Show)

data Recorrido = InOrden | PreOrden | PosOrden
    deriving (Eq, Show)

nVacios :: Arbol a -> Int
nVacios Vacio = 1
nVacios (AB _ i d) = nVacios i + nVacios d

refleja :: Arbol a -> Arbol a
refleja Vacio = Vacio
refleja (AB x i d) = AB x (refleja d) (refleja i)

minimo :: Ord a => Arbol a -> a
minimo (AB x Vacio Vacio) = x
minimo (AB x i Vacio) = min x (minimo i)
minimo (AB x Vacio d) = min x (minimo d)
minimo (AB x i d) = min x (min (minimo i) (minimo d))

recorrido :: Arbol a -> Recorrido -> [a]
recorrido Vacio _ = []

recorrido (AB x i d) InOrden =
    recorrido i InOrden ++ [x] ++ recorrido d InOrden

recorrido (AB x i d) PreOrden =
    [x] ++ recorrido i PreOrden ++ recorrido d PreOrden

recorrido (AB x i d) PosOrden =
    recorrido i PosOrden ++ recorrido d PosOrden ++ [x]

altura :: Arbol a -> Int
altura Vacio = 0
altura (AB _ i d) = 1 + max (altura i) (altura d)

esBalanceado :: Arbol a -> Bool
esBalanceado Vacio = True
esBalanceado (AB _ i d) =
    abs (altura i - altura d) <= 1
    && esBalanceado i
    && esBalanceado d

insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Vacio = AB x Vacio Vacio

insertar x (AB y i d)
    | x < y = AB y (insertar x i) d
    | x > y = AB y i (insertar x d)
    | otherwise = AB y i d

listaArbol :: Ord a => [a] -> Arbol a
listaArbol = foldl (flip insertar) Vacio
