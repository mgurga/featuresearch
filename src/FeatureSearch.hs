module FeatureSearch (start_feature_search, cross_validation_accuracy) where

import DataSet (Entry(..))

data SearchResults = SearchResults {
    best_accuracy :: Float,
    accuracys :: [Float],
    selected_features :: [Int]
} deriving (Show, Eq)

start_feature_search :: [Entry] -> IO SearchResults
start_feature_search ds = do
    -- create empty results to add to
    let emptyresults = SearchResults {best_accuracy = 0, accuracys = [], selected_features = []}
    fsr <- feature_search ds emptyresults
    return fsr

feature_search :: [Entry] -> SearchResults -> IO SearchResults
feature_search ds prev_results = do
    putStrLn ("searching " ++ (show (selected_features prev_results)))
    let prev_features = (selected_features prev_results)
    let features_to_search = 
         filter (\f -> (not (f `elem` prev_features))) [0..((length (features (ds!!0))) - 1)]

    putStr ("- considering feature ")
    all_paths <-
        mapM
        (\i -> do
            putStr ((show i) ++ " ... ")
            let new_feature_list = (selected_features prev_results) ++ [i]
            let acc = cross_validation_accuracy ds new_feature_list

            return SearchResults {
                best_accuracy = max (best_accuracy prev_results) acc,
                accuracys = (accuracys prev_results) ++ [acc],
                selected_features = new_feature_list
            }
        )
        features_to_search
    -- putStrLn ("finished searching feature list " ++ (show (selected_features prev_results)))
    let best_path = best_result all_paths
    putStrLn ("\n- best accuracy " ++ (show (best_accuracy best_path)))
    putStrLn ((show best_path))

    -- only call feature search on next layer if still features to test
    if (length features_to_search) == 1 then
        return (all_paths!!0)
    else
        feature_search ds best_path

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
                    (\neighbor best -> do
                        if neighbor == (ds!!dsi) then -- dont calculate distance of same dataset entry
                            best
                        else do
                            -- filter for features we want to test
                            let neighborfeats = map (\fi -> ((features neighbor)!!fi)) feature_list
                            let bestfeats = map (\fi -> ((features best)!!fi)) feature_list
                            -- update best neighbor if closer to target
                            if ((ndist neighborfeats targetfeats) < (ndist bestfeats targetfeats)) then
                                neighbor
                            else 
                                best
                    )
                    (ds!!0)
                    ds
            if targetcategory == (category nearestneighbor) then acc + 1 else acc
          ) 0 [0..((length ds) - 1)]
    correct_guesses / (fromIntegral (length ds))

best_result :: [SearchResults] -> SearchResults
best_result results =
    foldr
        (\r best -> (if ((best_accuracy r) > (best_accuracy best)) then r else best))
        (results!!0)
        results

ndist :: [Double] -> [Double] -> Double
ndist list1 list2 = do
    let innerlist =
            zipWith
                (\item1 item2 -> (item1 - item2)^(2 :: Integer))
                list1
                list2
    let distsum = sum innerlist
    sqrt distsum