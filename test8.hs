{-
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
-}

import Data.Char 
import Control.Monad
import System.IO
import Control.Exception.Base (handle)

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your lastName name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"    

main_putStr = do putStr "Hey, "
                 putStr "I'm "
                 putStr "Andy!\n" 

main_putChar = do putChar 't'
                  putChar 'e'
                  putChar 'h'

main_seq = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

main' = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors     

main_content = do
    contents <- getContents
    putStr (map toUpper contents)    

main'' = do
    handle <- openFile "Test_text" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle