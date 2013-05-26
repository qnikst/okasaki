import Data.List (foldl', foldl1)
import Test.Hspec
import Test.Hspec.QuickCheck

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = foldl' go [x] xs
  where
    go [] a = [a]
    go (a:as) b | a <= b    = a:go as b
                | otherwise = b:a:as
  
prop_sorted :: Ord a => [a] -> Bool
prop_sorted ls = insertionSort ls == insertionSort (insertionSort ls)

prop_asc :: Ord a => [a] -> Bool
prop_asc [] = True
prop_asc ls = fst $ foldl' go (True,h) t
  where
    (h:t) = (insertionSort ls)
    go (a,x) y = (a && x <= y, y)



sort_spec = describe "algorithm is correct" $ do
    prop "sort is stable" (prop_sorted :: [Int] -> Bool)
    prop "list is ascending" (prop_asc :: [Int] -> Bool)
