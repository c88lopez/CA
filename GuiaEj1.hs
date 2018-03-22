-- Ej 1
myMax :: (Int, Int) -> Int
myMax (x, y) = if (x > y) then x else y

-- Ej 2
myMax3 :: (Int, Int, Int) -> Int
myMax3 (x, y, z) = myMax(myMax(x, y), z)