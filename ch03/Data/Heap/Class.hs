{-# LANGUAGE RankNTypes #-}
module Data.Heap.Class
  where

import Data.List (foldl', sort)
import Test.Hspec
import Test.Hspec.QuickCheck

-- import Test.QuickCheck.Property

class Heap h  where
    empty     :: Ord a => h a
    isEmpty   :: Ord a => h a -> Bool
    insert    :: Ord a => a -> h a -> h a
    merge     :: Ord a => h a -> h a -> h a
    findMin   :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a

data T a = T

prop_min :: (Ord a, Show a, Heap h) => T (h a) -> [a] -> Bool
prop_min t [] = True -- TODO: should fail
prop_min t ls = go t (fromList ls) ls
  where
    go :: (Ord a, Show a, Heap h) => T (h a) -> h a -> [a] -> Bool
    go _ h ls = findMin h == minimum ls

prop_all :: (Ord a, Show a, Heap h) => T (h a) -> [a] -> Bool
prop_all t [] = True -- TODO: should fail
prop_all t ls = go t (fromList ls) ls
  where
    go :: (Ord a, Show a, Heap h) => T (h a) -> h a -> [a] -> Bool
    go _ h ls = toList h == (sort ls)

fromList :: (Heap h, Ord a) => [a] -> h a
fromList = foldl' (flip insert) empty

toList :: (Heap h, Ord a) => h a -> [a]
toList h | isEmpty h = []
         | otherwise = findMin h:toList (deleteMin h)

heap_spec :: Heap h => T (h Int) -> Spec
heap_spec x = describe "structure is a heap" $ do
    prop "structure contains all elements" (prop_all x :: [Int] -> Bool)
    prop "structure extranct correct min element" (prop_min x :: [Int] -> Bool)
