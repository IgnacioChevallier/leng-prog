module Main(main) where

import Frequencies
import System.Environment

main :: IO ()
main = do
   args <- getArgs
   if null args then do
       putStrLn "Please specify the filename"
       return ()
   else 
       freq (head args)
       
freq :: FilePath -> IO ()
freq fileName = do
  text <- readFile fileName
  printLines $ reverse $ (frequencies text)

printLines::[Frequency] -> IO ()
printLines [] = return ()
printLines ((w, c) : fs) = do
  putStrLn $ show c ++ ": " ++ show w
  printLines fs