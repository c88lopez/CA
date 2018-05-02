-- Ej 1
myEsVacia :: [a] -> Bool
myEsVacia [] = True
myEsVacia _ = False

-- Ej 2
myCabeza :: [a] -> a
myCabeza (x:_) = x

myCola :: [a] -> [a]
myCola (_:y) = y

-- Ej 3
myLong :: [a] -> Int
myLong [] = 0
myLong (_:y) = 1 + myLong (y)

mySuma :: [Int] -> Int
mySuma [] = 0
mySuma (x:y) = x + mySuma (y)

myMember :: (Int, [Int]) -> Bool
myMember (_, []) = False
myMember (x, y:z) = 
    if x == y
        then True
        else myMember(x, z)

myAppend :: ([a], [a]) -> [a]
myAppend ([], a) = a
myAppend (a, []) = a
myAppend (x:y, z) = (x:myAppend(y, z))

myTomar :: (Int, [a]) -> [a]
myTomar (0, z) = []
myTomar (x, y:z) = y:(myTomar(x-1, z))

myTerm :: (Int, [a]) -> a
myTerm (0, x:_) = x
myTerm (i, x:y) = myTerm (i-1, y)

myRev :: ([a]) -> [a]
myRev [] = []
myRev (x:y) = myAppend(myRev(y), x:[])

-- Ej 4
myDesdeHasta :: (Int, Int) -> [Int]
myDesdeHasta (min, max) =
    if min <= max
        then min:myDesdeHasta(min+1, max)
        else []

-- Ej 5
myProducto :: [Int] -> Int
myProducto [] = 1
myProducto (x:y) = x * myProducto (y)

myFactorialListas :: Int -> Int
myFactorialListas x = myProducto(myDesdeHasta(1, x))

-- Ej 6
--- i
myUltimo :: [a] -> a
myUltimo (x:[]) = x
myUltimo (_:y) = myUltimo(y)

--- ii
myTodosMenosUltimo :: [a] -> [a]
myTodosMenosUltimo (x:[]) = []
myTodosMenosUltimo (x:y) = x:(myTodosMenosUltimo(y))

-- Ej 7
myCapicua :: [Char] -> Bool
myCapicua [] = True
myCapicua (x:[]) = True
myCapicua (x:y) = 
    if x == myUltimo (y)
        then myCapicua(myTodosMenosUltimo(y))
        else False

-- Ej 8
myIntXor :: (Int, Int) -> Int
myIntXor (1, 1) = 0
myIntXor (x, y) = if x == 1 || y == 1 then 1 else 0

myXorl :: ([Int], [Int]) -> [Int]
myXorl (z, []) = z
myXorl ([], z) = z
myXorl (x, y) = 
    myAppend(
        myXorl(
            myTodosMenosUltimo(x), myTodosMenosUltimo(y)
        ), 
        myIntXor(myUltimo(x), myUltimo(y)):[]
    )

-- Ej 9
myPromedioSegundo :: [(String, Int, Char)] -> Int
myPromedioSegundo [] = 0
myPromedioSegundo ((_, edad, _):y) = edad + myPromedioSegundo(y)

myAverageAge :: [(String, Int, Char)] -> Int
myAverageAge [] = 0
myAverageAge ((nombre,edad,sexo):y) = 
    (edad + myPromedioSegundo(y)) `div` (1 + myLong y)

-- Ej 10
myEj10  :: [a] -> [a]
myEj10 [] = []
myEj10 (x:y) = x:(x:(myEj10(y)))

-- Ej 11


-- Ej 12
-- Cada vez que se llama a func(y:z) se quita un elemento a la lista en proceso.
-- Cuando la lista se queda sin elementos, queda [[], [...]], el cual no es un caso
-- contemplado en la definición original.
myEj12 :: [[a]] -> Int
myEj12 [[]] = 6
myEj12 ((x:y):z) = 2 + (myEj12(y:z))
myEj12 ([]:z) = 2 + myEj12(z)

-- Ej 13
myFlat :: [[a]] -> [a]
-- Condición final
myFlat [] = []

-- Se crea una lista donde la cabeza es el primer elemento de la primer lista
-- y la cola el resto (cola : z).
myFlat ((x:y):z) = x : myFlat(y:z)

-- Terminar con un listado significa procesar el siguiente
myFlat ([]:z) = myFlat(z)

-- Ej 14
myLongLL :: [[a]] -> Int
myLongLL [] = 0
myLongLL ((x:y):z) = 1 + myLongLL(y:z)
myLongLL ([]:z) = 0 + myLongLL(z)

-- Ej 15
myIntercalar :: ([a], [a]) -> [a]
myIntercalar ([], y) = y
myIntercalar (x, []) = x
myIntercalar ((x:y), (u:v)) = x : [u] ++ myIntercalar(y, v)