module Main (main) where

import DataSet (parse_text)

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs

    if length args == 1 then
        putStrLn ("reading dataset " ++ args!!0)
    else
        die "incorrect number of arguments, must provide dataset"

    dstext <- readFile (args!!0)
    let ds = parse_text dstext
    putStrLn ("header: " ++ (show (ds!!0)))

    


