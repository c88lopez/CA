--
-- input
-- package main;import "fmt";func main() {fmt.Println("Hello Wolrd!");}
--
-- output
-- package main;

-- import "fmt"

-- func main() {
--     fmt.Println
-- }

import System.IO

-- package Prettify where

-- Función principal de entrada. Al ejecutarla, estará esperando el contenido en la
--  siguiente linea, luego devolverá su resultado.
prettify :: IO()
prettify = do
    code <- getLine
    putStrLn("==================================\n" ++ parse code)

-- Esta funcion recibirá el string en la forma (primer caracter:resto string).
-- En base al primer caracter, tomará una decisión.
parse :: String -> String
parse "" = ""
parse (x:y) = 
    if charWhichIndents(x)
        then 
            if startsBlock(x)
                then x:"\n    " ++ parse(y)
                else 
                    if endsBlock(x)
                        then "\n" ++ [x] ++ parse(y) 
                        else x : "\n" ++ parse(y) 
        else x:parse(y)

-- Define cuales son los caracteres que definen un cambio de identación.
charWhichIndents :: Char -> Bool
charWhichIndents '{' = True
charWhichIndents '}' = True
charWhichIndents ';' = True
charWhichIndents _ = False

-- Caracteres que abren un bloque.
startsBlock :: Char -> Bool
startsBlock '{' = True
startsBlock _ = False

-- Caracteres que cierran un bloque.
endsBlock :: Char -> Bool
endsBlock '}' = True
endsBlock _ = False
