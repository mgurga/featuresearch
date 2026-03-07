module FeatureSearch (start_feature_search, cross_validation_accuracy) where

import DataSet (Entry(..))
import qualified Data.Set as Set

data SearchResults = SearchResults {
    best_accuracy :: Float,
    accuracys :: [Float],
    selected_features :: [Int]
} deriving (Show, Eq)

start_feature_search :: [Entry] -> IO SearchResults
start_feature_search ds = do
    results <-
         mapM (\fi -> do
            fsr <- feature_search ds [] fi
            putStrLn ("calculated results for " ++ (show [fi]) ++ " got " ++ (show fsr))
            return fsr
         )
         [0..((length (features (ds!!0))) - 1)]
    
    -- return result with best accuracy
    let best_res =
         foldr
            (\r1 r2 -> (if ((best_accuracy r1) > (best_accuracy r2)) then r1 else r2))
            (results!!0)
            results

    return best_res

feature_search :: [Entry] -> [Int] -> Int -> IO SearchResults
feature_search ds feature_list next_feature = do
    let new_feature_list = feature_list ++ [next_feature]
    --let next_feature_list = -- creates the next feature list removing duplicates
    --     filter (\fi -> (not (fi `elem` new_feature_list))) [0..((length (features (ds!!0))) - 1)]
    let contains_duplicates = (length (Set.fromList new_feature_list)) /= (length new_feature_list)

    -- stop early if dupicaltes in feature list
    if (length new_feature_list) == (length (features (ds!!0))) || contains_duplicates then
        return SearchResults {best_accuracy = 0, accuracys = [], selected_features = []}
    else do
        let acc = cross_validation_accuracy ds new_feature_list
        putStrLn ("next feature list " ++ (show new_feature_list))
        results <-
            mapM (\fi -> (feature_search ds new_feature_list fi)) new_feature_list
        let best_results =
                foldr
                    (\r1 r2 -> (if ((best_accuracy r1) > (best_accuracy r2)) then r1 else r2))
                    (results!!0)
                    results
        let best_acc = max (best_accuracy best_results) acc

        return SearchResults {
            best_accuracy = best_acc,
            accuracys = (accuracys best_results) ++ [best_acc],
            selected_features = (selected_features best_results) ++ [next_feature]
        }

cross_validation_accuracy :: [Entry] -> [Int] -> Float
cross_validation_accuracy _ [] = 0.5 -- if feature list is empty default accuracy is 0.5
cross_validation_accuracy ds feature_list = do
    let correct_guesses =
         foldr
          (\dsi acc -> do -- loop over dataset with index dsi and accumulator
            -- set target values based on current dataset entry
            let targetcategory = (category (ds!!dsi)) -- 1 or 2
            let targetfeats = map (\fi -> ((features (ds!!dsi))!!fi)) feature_list
            let nearestneighbor =
                  foldr
                    (\n1 n2 -> do
                        if n1 == (ds!!dsi) then -- dont calculate distance of same dataset entry
                            n2
                        else do
                            let n1feats = map (\fi -> ((features n1)!!fi)) feature_list
                            let n2feats = map (\fi -> ((features n2)!!fi)) feature_list
                            if ((ndist n1feats targetfeats) < (ndist n2feats targetfeats)) then
                                n1 -- calculate distance b/w target
                            else 
                                n2
                    )
                    (ds!!0)
                    ds
            if targetcategory == (category nearestneighbor) then acc + 1 else acc
          ) 0 [0..((length ds) - 1)]
    correct_guesses / (fromIntegral (length ds))

ndist :: [Double] -> [Double] -> Double
ndist list1 list2 = do
    let innerlist =
            zipWith
                (\item1 item2 -> (item1 - item2)^(2 :: Integer))
                list1
                list2
    let distsum = sum innerlist
    sqrt distsum