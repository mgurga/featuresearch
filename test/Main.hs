module Main (main) where

import Test.Hspec
import DataSet (parse_text)
import FeatureSearch (cross_validation_accuracy, ndist)

main :: IO ()
main = hspec $ do
    describe "Feature Search Tests" $ do
        it "Sanity Check 1" $ do
            ds1text <- readFile "SanityCheck_DataSet__1.txt"
            let sanity1ds = parse_text ds1text
            let acc = cross_validation_accuracy sanity1ds [7, 10, 12]
            acc `shouldSatisfy` (\a -> a <= 0.96 && a >= 0.94) -- accuracy should be 0.95 +- 0.1
        
        it "Sanity Check 2" $ do
            ds2text <- readFile "SanityCheckDataSet__2.txt"
            let sanity2ds = parse_text ds2text
            let acc = cross_validation_accuracy sanity2ds [10, 8, 2]
            acc `shouldSatisfy` (\a -> a <= 0.97 && a >= 0.95) -- accuracy should be 0.96 +- 0.1

    describe "Euclidean Distance Tests" $ do
        it "1 Dimension" $ do
            (ndist [1] [3]) `shouldBe` 2.0
        it "4 Dimensions" $ do
            (ndist [1, 2, 3, 4] [5, 6, 7, 8]) `shouldBe` 8.0
        it "Negative 5 Dimensions" $ do
            (ndist [1, -5, -2, 4, 9] [13, 3, -27, -2, 3])
                `shouldSatisfy` (\d -> d <= 31 && d >= 29) -- should be 30