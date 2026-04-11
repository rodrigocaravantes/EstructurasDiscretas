module Practica5 where

import Data.Char

-- hollerBack
hollerBack :: String -> String
hollerBack [] = []
hollerBack (x:xs) = toUpper x : hollerBack xs

-- decimal_binario
decimal_binario :: Int -> [Int]
decimal_binario 0 = [0]
decimal_binario n = reverse (binarioAux n)

binarioAux :: Int -> [Int]
binarioAux 0 = []
binarioAux n = (n `mod` 2) : binarioAux (n `div` 2)

-- replica
replica :: Int -> Int -> [Int]
replica x 0 = []
replica x n = x : replica x (n-1)

-- recuperaElemento
recuperaElemento :: [a] -> Int -> a
recuperaElemento (x:xs) 0 = x
recuperaElemento (x:xs) n = recuperaElemento xs (n-1)

-- rota
rota :: [a] -> Int -> [a]
rota xs 0 = xs
rota (x:xs) n = rota (xs ++ [x]) (n-1)

-- extranio
extranio :: Int -> [Int]
extranio 1 = [1]
extranio n
    | even n = n : extranio (n `div` 2)
    | otherwise = n : extranio (3*n + 1)
