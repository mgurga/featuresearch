module DataSet (parse_text) where
import GHC.Float (double2Int)

data Entry = Entry {
    category :: Int,
    features :: [Double]
} deriving (Show, Eq)

parse_text :: String -> [Entry]
parse_text dstext =
    map
        (\line -> do
            let nums = words line
            Entry {
                category=(double2Int (read (nums!!0) :: Double)),
                features=(map (\f -> (read f :: Double)) (drop 1 nums))
            })
        (lines dstext)