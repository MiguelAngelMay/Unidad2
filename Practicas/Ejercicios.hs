--ejercicio 1
vlePotenc :: Int -> Int -> Int 
vlePotenc  a b = a^b

--ejercicio 3
segundosCAlcula :: Integer->(Integer,Integer,Integer)
segundosCAlcula s = (horas, minutos, segundos) where 
 horas = div s 3600
 ss = mod s 3600
 minutos = div ss 60
 segundos = mod ss 60

 --ejercicio 4
mayor :: Integer -> Integer -> Integer
mayor x y = div ((x + y) + abs(x - y)) 2

mayor3 :: Integer -> Integer -> Integer -> Integer
mayor3 x y z = mayor x (mayor y z)

mayor4 :: Integer -> Integer -> Integer -> Integer -> Integer
mayor4 w x y z = mayor (mayor w x) (mayor y z)

--ejercicio 5
sumaNumeros :: Int -> Int
sumaNumeros 0 = 0 
sumaNumeros n = n + sumaNumeros(n-1)

--ejercicio 7
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada (x:y:zs)
	|x <= y = estaOrdenada (y:zs)
	|otherwise = False  