data Entero = Zero | Succ Entero | Neg Entero deriving (Eq,Show)

-- suma de enteros
sumaEnt :: Entero -> Entero -> Entero
sumaEnt Zero n = n
sumaEnt n Zero = n
sumaEnt (Succ n) m = Succ (sumaEnt n m)
sumaEnt (Neg n) m = Neg (sumaEnt n m)

-- multiplicacion
multiEnt :: Entero -> Entero -> Entero
multiEnt Zero _ = Zero
multiEnt _ Zero = Zero
multiEnt (Succ n) m = sumaEnt m (multiEnt n m)
multiEnt (Neg n) m = Neg (multiEnt n m)
