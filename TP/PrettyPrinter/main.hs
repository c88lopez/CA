--
-- input
-- package main;import "fmt";func main() {fmt.Println("Hello Wolrd!")}
--
-- output
-- package main;

-- import "fmt"

-- func main() {
--     fmt.Println
-- }

import System.IO

-- package Prettify where

prettify :: IO()
prettify = do
    code <- getLine
    putStrLn("\n" ++ parse code)

parse :: String -> String
parse "" = ""
parse (x:y) = 
    if charWhichIndents(x)
        then x:"\n" ++ parse(y)
        else x:parse(y)

charWhichIndents :: Char -> Bool
charWhichIndents '{' = True
-- charWhichIndents '}' = True
charWhichIndents ';' = True
charWhichIndents _ = False
