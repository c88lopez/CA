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