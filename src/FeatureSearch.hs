module FeatureSearch (
    start_forward_feature_search,
    start_backward_feature_search,
    cross_validation_accuracy,
    ndist
) where

import DataSet (Entry(..))

data SearchResults = SearchResults {
    best_accuracy :: Float,
    accuracys :: [Float],
    selected_features :: [Int],
    to_search :: [Int] -- only used for backward search
} deriving (Show, Eq)

start_forward_feature_search :: [Entry] -> IO SearchResults
start_forward_feature_search ds = do
    -- create empty results to add to
    let emptyresults = 
         SearchResults {best_accuracy = 0, accuracys = [], selected_features = [], to_search = []}
    fsr <- forward_feature_search ds emptyresults
    return fsr

start_backward_feature_search :: [Entry] -> IO SearchResults
start_backward_feature_search ds = do
    -- start by getting accuracy of all features enabled
    let allacc = cross_validation_accuracy ds [1..(length (features (ds!!0)))]
    let emptyresults = SearchResults {
        best_accuracy = allacc,
        accuracys = [(allacc)],
        selected_features = [],
        to_search = [1..(length (features (ds!!0)))]
    }
    fsr <- backward_feature_search ds emptyresults
    return fsr

forward_feature_search :: [Entry] -> SearchResults -> IO SearchResults
forward_feature_search ds prev_results = do
    putStrLn ("searching " ++ (show (selected_features prev_results)))
    let prev_features = (selected_features prev_results)
    -- 1 indexed feature list
    let features_to_search = 
         filter (\f -> (not (f `elem` prev_features))) [1..(length (features (ds!!0)))]

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
                selected_features = new_feature_list,
                to_search = []
            }
        )
        features_to_search
    -- putStrLn ("finished searching feature list " ++ (show (selected_features prev_results)))
    let best_path = best_result all_paths
    putStrLn ("\n- best accuracy " ++ (show (best_accuracy best_path)))

    -- only call feature search on next layer if still features to test
    if (length features_to_search) == 1 then do
        let feat_acc = reverse (zip (selected_features best_path) (accuracys best_path))
        feature_accuracy_table feat_acc
        return best_path
    else
        forward_feature_search ds best_path

backward_feature_search :: [Entry] -> SearchResults -> IO SearchResults
backward_feature_search ds prev_results = do
    putStrLn ("searching " ++ (show (selected_features prev_results)))
    -- 1 indexed feature list
    -- start will full feature list and recursivly remove the worst features
    let features_to_search = (to_search prev_results)

    putStr ("- considering feature ")
    all_paths <-
        mapM
        (\i -> do
            putStr ((show i) ++ " ... ")
            -- remove i from feature list
            let new_feature_list =
                    filter (\f -> (not (f == i))) (to_search prev_results)
            let acc = cross_validation_accuracy ds new_feature_list

            return SearchResults {
                best_accuracy = max acc (best_accuracy prev_results),
                accuracys =
                    -- avoid adding empty feature list accuracy to final accuracy list
                    (if new_feature_list /= [] then
                        (accuracys prev_results) ++ [acc]
                    else
                        (accuracys prev_results)),
                selected_features = (selected_features prev_results) ++ [i],
                to_search = new_feature_list
            }
        )
        features_to_search
    -- putStrLn ("finished searching feature list " ++ (show (selected_features prev_results)))
    let best_path = best_result all_paths
    putStrLn ("\n- accuracy list " ++ (show (accuracys best_path)))

    -- find removed feature
    let feature_difference =
            filter (\fi -> not (fi `elem` (to_search best_path))) features_to_search
    putStrLn ("- removing feature " ++ (show (feature_difference!!0)))

    -- only call feature search on next layer if still features to test
    if (length features_to_search) == 1 then do
        feature_accuracy_table (zip (selected_features best_path) (accuracys best_path))
        return best_path
    else
        backward_feature_search ds best_path

cross_validation_accuracy :: [Entry] -> [Int] -> Float
cross_validation_accuracy _ [] = 0.5 -- if feature list is empty default accuracy is 0.5
cross_validation_accuracy ds feature_list = do
    let correct_guesses =
         foldr
          (\dsi acc -> do -- loop over dataset with index dsi and accumulator
            -- set target values based on current dataset entry
            let targetcategory = (category (ds!!dsi)) -- 1 or 2
            let targetfeats = map (\fi -> ((features (ds!!dsi))!!(fi - 1))) feature_list
            let nearestneighbor =
                  foldr
                    (\neighbor best -> do
                        if neighbor == (ds!!dsi) then -- dont calculate distance of same dataset entry
                            best
                        else do
                            -- filter for features we want to test
                            let neighborfeats =
                                 map (\fi -> ((features neighbor)!!(fi - 1))) feature_list
                            let bestfeats =
                                 map (\fi -> ((features best)!!(fi - 1))) feature_list
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
    correct_guesses / (fromIntegral ((length ds) - 1))

-- return result with best accuracy
best_result :: [SearchResults] -> SearchResults
best_result results =
    foldr
        (\r best -> (if ((best_accuracy r) > (best_accuracy best)) then r else best))
        (results!!0)
        results

-- euclidean distance calculator
ndist :: [Double] -> [Double] -> Double
ndist list1 list2 = do
    let innerlist =
            zipWith
                (\item1 item2 -> (item1 - item2)^(2 :: Integer))
                list1
                list2
    let distsum = sum innerlist
    sqrt distsum

feature_accuracy_table :: [(Int, Float)] -> IO ()
feature_accuracy_table feat_acc = do
    let featrow = foldr (\(f, _) acc -> acc ++ ((show f) ++ "\t")) "" feat_acc
    let accrow = foldr (\(_, a) acc -> acc ++ ((take 5 (show a)) ++ "\t")) "" feat_acc
    putStrLn ("\n" ++ featrow ++ "\n" ++ accrow ++ "\n")
    return ()