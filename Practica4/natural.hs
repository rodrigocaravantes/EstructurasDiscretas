data Natural = Cero | S Natural deriving (Eq,Show)

-- convierte entero a Natural
a_natural :: Int -> Natural
a_natural 0 = Cero
a_natural n = S (a_natural (n-1))

-- convierte Natural a entero
a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S n) = 1 + a_entero n

-- potencia con naturales
potenciaNat :: Natural -> Natural -> Natural
potenciaNat n Cero = S Cero
potenciaNat n (S m) = multNat n (potenciaNat n m)

-- multiplicacion auxiliar
multNat :: Natural -> Natural -> Natural
multNat n Cero = Cero
multNat n (S m) = sumaNat n (multNat n m)

-- suma auxiliar
sumaNat :: Natural -> Natural -> Natural
sumaNat Cero n = n
sumaNat (S n) m = S (sumaNat n m)

-- factorial
facNat :: Natural -> Natural
facNat Cero = S Cero
facNat (S n) = multNat (S n) (facNat n)
