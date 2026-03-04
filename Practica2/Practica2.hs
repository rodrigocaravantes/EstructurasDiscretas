-- Practica 2
-- Rodrigo Caravantes Roman
-- Introduccion a Haskell

import Data.Bits

-- agrega hello al texto
sayHello :: String -> String
sayHello nombre = "Hello, " ++ nombre ++ "!"

-- saca el 10 porciento
calcularPropina :: Double -> Double
calcularPropina cuenta = cuenta * 0.10

-- regresa el numero mas chico
menor :: (Int,Int,Int) -> Int
menor (a,b,c) =
    if a <= b && a <= c then a
    else if b <= a && b <= c then b
    else c

-- decide cual texto regresar
decide :: Bool -> String -> String -> String
decide valor x y =
    if valor then x else y

-- revisa si estan en orden descendente
esDescendiente :: Int -> Int -> Int -> Int -> Bool
esDescendiente x y z w =
    if x > y && y > z && z > w then True else False

-- revisa si es divisible
esDivisible :: Int -> Int -> String
esDivisible x y =
    if y == 0 then "no se puede dividir entre cero"
    else if mod x y == 0
        then show x ++ " es divisible por " ++ show y
        else show x ++ " no es divisible por " ++ show y

-- revisa si es par usando bits
esPar :: Int -> Bool
esPar n =
    if (n .&. 1) == 0 then True else False

-- formula de la hipotenusa
hipotenusa :: Float -> Float -> Float
hipotenusa b h =
    sqrt (b*b + h*h)

-- pendiente entre dos puntos
pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (x1,y1) (x2,y2) =
    (y2 - y1) / (x2 - x1)

-- distancia entre dos puntos
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- extra
cuadrados :: [Int] -> [Int]
cuadrados xs =
    [x*x | x <- xs]
