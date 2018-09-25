-- Faculty
fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = n * fac (n-1)

-- Faculty tail recursion
fac_it :: Integer -> Integer
fac_it 1 = 1
fac_it n = aux n 1
  where
    aux :: Integer -> Integer -> Integer
    aux 0 m = m
    aux n m = aux (n-1) (n*m)
