import Data.List
import Test.QuickCheck

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes a@(x:xs) = a:suffixes xs

prop_len x = length x + 1 == length (suffixes x)
prop_in [] = True
prop_in  x = 
  case suffixes x of
      [] -> True
      [s] -> True
      t@(s:s2:ss) -> all (`isSuffixOf` x) t && prop_in s2
prop_empty x = [] `elem` suffixes x

main = do
  quickCheck (prop_len :: [Int] -> Bool)
  quickCheck (prop_in  :: [Int] -> Bool)
  quickCheck (prop_empty :: [Int] -> Bool)
