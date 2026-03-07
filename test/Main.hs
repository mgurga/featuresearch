module Main (main) where

import DataSet (parse_text)
import FeatureSearch (cross_validation_accuracy)

main :: IO ()
main = do
    dstext <- readFile "SanityCheck_DataSet__1.txt"
    let ds = parse_text dstext
    let acc = cross_validation_accuracy ds [0]
    putStrLn ("accuracy " ++ (show acc))
