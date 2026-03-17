module Main (main) where

import DataSet (parse_text)

import System.Environment (getArgs)
import System.Exit (die)
import FeatureSearch (start_forward_feature_search, start_backward_feature_search)
import System.TimeIt (timeItT)

print_help :: IO ()
print_help = do
    putStrLn "featuresearch - find features in csv datasets"
    putStrLn "Usage:"
    putStrLn "\tfeaturesearch --help\t\t\tprint help"
    putStrLn "\tfeaturesearch data.txt\t\t\trun forward selection on data.txt"
    putStrLn "\tfeaturesearch backward data.txt\t\trun backward elimination on data.txt"
    putStrLn "\tfeaturesearch forward data.txt\t\trun forward selection on data.txt"

main :: IO ()
main = do
    args <- getArgs

    if length args == 1 && (args!!0 == "--help" || args!!0 == "-h" || args!!0 == "help") then
        print_help
    else if length args == 1 then do
        putStrLn ("reading dataset " ++ args!!0)

        dstext <- readFile (args!!0)
        let ds = parse_text dstext
        putStrLn ("header " ++ (show (ds!!0)))

        (time, res) <- timeItT (start_forward_feature_search ds)
        putStrLn ("forward search took " ++ (show time) ++ "s\nfinal results " ++ (show res))
    else if length args == 2 then do
        putStrLn ("reading dataset " ++ args!!1)

        dstext <- readFile (args!!1)
        let ds = parse_text dstext
        putStrLn ("header " ++ (show (ds!!0)))

        if (args!!0) == "forward" then do
            (time, res) <- timeItT (start_forward_feature_search ds)
            putStrLn ("forward search took " ++ (show time) ++ "s\nfinal results " ++ (show res))
        else if (args!!0) == "backward" then do
            (time, res) <- timeItT (start_backward_feature_search ds)
            putStrLn ("backward search took " ++ (show time) ++ "s\nfinal results " ++ (show res))
        else
            die ("invalid search option '" ++ (args!!0) ++ "' valid options are 'forward' and 'backward")
    else do
        print_help
        die "incorrect number of arguments, must provide dataset"