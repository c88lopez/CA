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