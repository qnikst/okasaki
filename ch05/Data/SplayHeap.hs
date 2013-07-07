module Data.SplayHeap
  where

import Test.QuickCheck
import Data.List hiding (insert)
import Test.Hspec
import Test.Hspec.QuickCheck

data Tree a = E | T (Tree a) a (Tree a) deriving (Eq,Show)

insert :: (Ord a) =>  a -> Tree a  -> Tree a
insert a t = T (smaller t a) a (bigger t a)
  where
    bigger E _ = E
    bigger (T l x r) pivot
      | x <= pivot = bigger r pivot
      | otherwise = case l of 
                        E -> T E x r
                        T l2 x2 r2 
                            | x2 <= pivot -> T (bigger r2 pivot) x r
                            | otherwise   -> T (bigger l2 pivot) x2 (T r2 x r)
    smaller E _ = E
    smaller (T l x r) pivot
      | x > pivot = smaller l pivot
      | otherwise = case r of
                       E -> T l x E
                       T l2 x2 r2
                          | x2 > pivot -> T l x (smaller l2 pivot)
                          | otherwise  -> T (T l x l2) x2 (smaller r2 pivot)


-- Utils:
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl' (flip insert) E 

toList :: (Ord a) => Tree a -> [a]
toList E = []
toList (T l x r) = toList l ++ x:toList r

-- Properties:
prop_contains_all :: (Ord a) => [a] -> Bool
prop_contains_all xs = sort xs == toList (fromList xs)

prop_inv1 :: (Ord a) => Tree a -> Bool
prop_inv1 (T l@(T _ a _) x r@(T _ b _)) = a < x && x < b && prop_inv1 l && prop_inv1 r
prop_inv1 _ = True

test_inv1 :: (Ord a) => [a] -> Bool
test_inv1 = prop_inv1 . fromList
{-
prop_real_rank :: (Ord a) => [a] -> Bool
prop_real_rank = inv_real_rank . fromList

leftish_heap_unit :: T (LeftishHeap a)
leftish_heap_unit = T
-}

splayheap_spec :: Spec
splayheap_spec = do
    describe "structure tests" $ do
        prop "contains all elements" $ (prop_contains_all :: [Int] -> Bool)
    describe "heap tests" $ do
        prop "heap invariant" $ (test_inv1 :: [Int] -> Bool)
--        prop "leftish heap is balanced" (prop_balanced :: [Int] -> Bool)
--        prop "leftish heap has correct rank" (prop_real_rank :: [Int] -> Bool)

{-
leftish_heap_tests = do
    heap_spec (T :: T (LeftishHeap Int))
    leftish_heap_spec
    -}
