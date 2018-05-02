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

-- Si bien `x` es una lista (y uno tiende a usar ++ para "sumar" listas), es 
-- incorrecto usar `++` porque los argumentos deben ser del mismo tipo.
myIntercalar ((x:y), (u:v)) = x : [u] ++ myIntercalar(y, v)

-- Ej 16
myMerge :: ([Int], [Int]) -> [Int]
myMerge ([], x) = x
myMerge (x, []) = x
myMerge ((x:y), (u:v)) = 
    if x < u
        then x : myMerge(y, u:v)
        else u : myMerge(x:y, v)

-- Ej 17
mySearchAndReplace :: (Int, Int, [Int]) -> [Int]
mySearchAndReplace (_, _, []) = []
mySearchAndReplace (search, replace, (x:y)) = 
    if search == x
        then replace : mySearchAndReplace(search, replace, y)
        else x : mySearchAndReplace(search, replace, y)

mySearchAndReplaceOne :: (Int, Int, [Int]) -> [Int]
mySearchAndReplaceOne (_, _, []) = []
mySearchAndReplaceOne (search, replace, (x:y)) = 
    if search == x
        then replace : y
        else x : mySearchAndReplaceOne(search, replace, y)

-- Ej 18
myDecHexMap :: Int -> Char
myDecHexMap 0 = '0'
myDecHexMap 1 = '1'
myDecHexMap 2 = '2'
myDecHexMap 3 = '3'
myDecHexMap 4 = '4'
myDecHexMap 5 = '5'
myDecHexMap 6 = '6'
myDecHexMap 7 = '7'
myDecHexMap 8 = '8'
myDecHexMap 9 = '9'
myDecHexMap 10 = 'A'
myDecHexMap 11 = 'B'
myDecHexMap 12 = 'C'
myDecHexMap 13 = 'D'
myDecHexMap 14 = 'E'
myDecHexMap 15 = 'F'

myDecToHex :: Int -> String
myDecToHex x =
    if x < 16
        then [myDecHexMap(x)]
        else myDecToHex(x `div` 16) ++ [myDecHexMap(x `mod` 16)]

-- Ej 19
myContar :: (Int, [Int]) -> Int
myContar (_, []) = 0
myContar (x, (y:z)) = 
    if x == y
        then 1 + myContar(x, z)
        else myContar(x, z)

myMayoria :: [Int] -> Int
myMayoria (x:[]) = 1
myMayoria (x:y) = 
    if myContar(x, y) >= myMayoria(y)
        then x
        else myMayoria(y)