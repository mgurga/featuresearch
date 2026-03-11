module Main (main) where

import DataSet (parse_text)

import System.Environment (getArgs)
import System.Exit (die)
import FeatureSearch (start_forward_feature_search, start_backward_feature_search)

main :: IO ()
main = do
    args <- getArgs

    if length args == 1 then do
        putStrLn ("reading dataset " ++ args!!0)

        dstext <- readFile (args!!0)
        let ds = parse_text dstext
        putStrLn ("header " ++ (show (ds!!0)))

        res <- start_forward_feature_search ds
        putStrLn ("final results " ++ (show res))
    else if length args == 2 then do
        putStrLn ("reading dataset " ++ args!!1)

        dstext <- readFile (args!!1)
        let ds = parse_text dstext
        putStrLn ("header " ++ (show (ds!!0)))

        if (args!!0) == "forward" then do
            res <- start_forward_feature_search ds
            putStrLn ("final results " ++ (show res))
        else if (args!!0) == "backward" then do
            res <- start_backward_feature_search ds
            putStrLn ("final results " ++ (show res))
        else
            die ("invalid search option '" ++ (args!!0) ++ "' valid options are 'forward' and 'backward")
    else
        die "incorrect number of arguments, must provide dataset"