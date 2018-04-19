-- Ej 1
myMax :: (Int, Int) -> Int
myMax (x, y) = if (x > y) then x else y

-- Ej 2
myMax3 :: (Int, Int, Int) -> Int
myMax3 (x, y, z) = myMax(myMax(x, y), z)

-- Ej 3 @todo
myMin :: (Int, Int) -> Int
myMin (x, y) = if (x < y) then x else y

myMin3 :: (Int, Int, Int) -> Int
myMin3 (x, y, z) = myMin(myMin(x, y), z)

myOrder3 :: (Int, Int, Int) -> (Int, Int, Int) 
myOrder3 (x, y, z) = (
    myMin3(x, y, z), 
    (y), 
    myMax3(x, y, z))
   
-- Ej 4
myAnd :: (Bool, Bool) -> Bool
myAnd (True, y) = y
myAnd (_, _) = False

myOr :: (Bool, Bool) -> Bool
myOr (True, y) = True
myOr (_, y) = y

myNot :: Bool -> Bool
myNot True = False
myNot _ = True

myXor :: (Bool, Bool) -> Bool
myXor (True, True) = False
myXor (x, y) = myOr(x, y)

myImplica :: (Bool, Bool) -> Bool
myImplica (True, y) = y
myImplica (_, _) = True

myEquivalente :: (Bool, Bool) -> Bool
myEquivalente (True, y) = y
myEquivalente (_, y) = myNot(y)

-- Ej 5
mySumaComplejo :: ((Int, Int), (Int, Int)) -> (Int, Int)
mySumaComplejo ((n1, i1), (n2, i2)) = ((n1 + n2), (i1 + i2))

myMultComplejo :: ((Int, Int), (Int, Int)) -> (Int, Int)
myMultComplejo ((n1, i1), (n2, i2)) = (
    (n1 * n2) - (i1 * i2), 
    (n1*i2) + (i1 * n2))

-- Ej 6


-- Ej 7
myFib :: Int -> Int
myFib 0 = 0
myFib 1 = 1
myFib x = myFib(x-1) + myFib(x-2)

-- Ej 8
myEsPar :: Int -> Bool
myEsPar 0 = True
myEsPar 1 = False
myEsPar x = not(myEsPar(x-1))

-- Ej 9
myEsDivPor :: (Int, Int) -> Bool
myEsDivPor (_, 0) = False
myEsDivPor (0, _) = True
myEsDivPor (x, y) = 
    if x == y 
        then True
        else if x < y 
            then False
            else myEsDivPor(x-y, y)
                        
-- Ej 10
myEsDivisiblePor :: (Int, Int, Int) -> Bool
myEsDivisiblePor (0, _, _) = True
myEsDivisiblePor (_, 1, _) = True
myEsDivisiblePor (x, y, z) = 
    if y > z
        then False
        else if myEsDivPor (x, z)
            then True
            else myEsDivisiblePor (x, y, z-1)

myEsPrimo :: Int -> Bool
myEsPrimo 1 = False
myEsPrimo x = not(myEsDivisiblePor(x, 2, x-1))

-- Ej 11
-- En la carpeta

-- Ej 12
-- En la carpeta

-- Ej 13
myTerminaEnSiete :: (Int) -> Bool
myTerminaEnSiete x = 
    if x > 10
        then myTerminaEnSiete (x-10)
        else x == 7

myCuantosSietes :: (Int, Int) -> Int
myCuantosSietes (x, y) =
    if x <= y
        then if myTerminaEnSiete (y)
            then 1 + myCuantosSietes(x, y-10)
            else 0 + myCuantosSietes(x, y-1)
        else 0

-- Ej 14
--- a
mySeCuelgaMayor5 :: Int -> Bool
mySeCuelgaMayor5 x = 
    if x > 5
        then mySeCuelgaMayor5 x
        else True
--- b
mySeCuelgaF :: Int -> Bool
mySeCuelgaF x =
    if x > 25
        then mySeCuelgaF x
        else True

mySeCuelgaG :: Int -> Bool
mySeCuelgaG x =
    if x < 5
        then mySeCuelgaG x
        else True

mySeCuelgaH :: Int -> Bool
mySeCuelgaH x = mySeCuelgaF (x) && mySeCuelgaG (x)

-- Ej 15
myFactorial :: Int -> Int
myFactorial 0 = 1
myFactorial x = 
    if x > 0
        then x * myFactorial (x - 1)
        else x * myFactorial (x + 1)