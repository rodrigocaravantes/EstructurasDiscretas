data Matrioska = Mati | Cont Matrioska deriving (Eq,Show)

-- comparar matrioskas
mayorIgual :: Matrioska -> Matrioska -> Bool
mayorIgual Mati Mati = True
mayorIgual Mati (Cont _) = False
mayorIgual (Cont _) Mati = True
mayorIgual (Cont a) (Cont b) = mayorIgual a b

-- aplana matrioska
aplana :: Matrioska -> [Matrioska]
aplana Mati = [Mati]
aplana (Cont m) = (Cont m) : aplana m
