-- Ej adicional, maximo comun divisor
myMaxComDiv :: (Int, Int) -> Int
myMaxComDiv (m, n) = if (m==n) 
    then m 
    else if (m>n) 
        then myMaxComDiv(m-n, n) 
        else myMaxComDiv(m, n-m)

myMenoresOIg :: ([Int], Int) -> [Int]
myMenoresOIg ([], _) = []
myMenoresOIg (x:y, e) = 
    if x <= e
        then x:myMenoresOIg(y, e)
        else myMenoresOIg(y, e)

myMayores :: ([Int], Int) -> [Int]
myMayores ([], _) = []
myMayores (x:y, e) = 
    if x > e
        then x:myMayores(y, e)
        else myMayores(y, e)

myQuickSort :: [Int] -> [Int]
myQuickSort [] = []
myQuickSort (x:y) = 
    myQuickSort(
        myMenoresOIg(y, x)
    )
    ++
    x:myQuickSort(
        myMayores(y, x)
    )
    