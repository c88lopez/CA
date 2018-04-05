-- Ej adicional, maximo comun divisor
myMaxComDiv :: (Int, Int) -> Int
myMaxComDiv (m, n) = if (m==n) 
    then m 
    else if (m>n) 
        then myMaxComDiv(m-n, n) 
        else myMaxComDiv(m, n-m)

-- Fib
myFib :: Int -> Int
myFib 0 = 0
myFib 1 = 1
myFib x = myFib(x-1) + myFib(x-2)