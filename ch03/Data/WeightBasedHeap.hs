{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.WeightBasedHeap where

import Data.Heap.Class
import Test.Hspec
import Test.Hspec.QuickCheck

data WeightBasedHeap a = E | H Int a (WeightBasedHeap a) (WeightBasedHeap a) deriving (Eq, Show)

instance Heap WeightBasedHeap where
    empty   = E
    isEmpty E = True
    isEmpty _ = False
    insert  x h = singleton x `merge` h
    merge   h E = h
    merge   E h = h
    merge   h1@(H s1 x l1 r1) h2@(H s2 y l2 r2) = 
        let (x',l',r',h') = if x < y                          -- only top-down
                                then (x,l1,r1,h2) 
                                else (y,l2,r2,h1)
            (l'',r'')     = if size l' > size r' + size h'    -- only top-down
                                then (l',r' `merge` h')
                                else (r' `merge` h', l')
        in H (s1+s2) x' l'' r''
    findMin E = error "empty"
    findMin (H _ x _ _) = x
    deleteMin E = error "empty"
    deleteMin (H _ _ l r) = l `merge` r

singleton :: a -> WeightBasedHeap a
singleton a = H 1 a E E

size E = 0
size (H w _ _ _) = w

makeT x a b = if size a >= size b
                  then H (size b+1) x a b
                  else H (size a+1) x b a

inv_weight:: WeightBasedHeap a -> Bool
inv_weight E = True
inv_weight (H _ _ E E) = True
inv_weight (H _ _ E h) = False
inv_weight (H _ _ h1 h2) = 
        size h1 >= size h2 
        && inv_weight h1 
        && inv_weight h2

prop_balanced :: (Ord a) => [a] -> Bool
prop_balanced = inv_weight . fromList

weight_based_heap_spec :: Spec
weight_based_heap_spec = do
    describe "weight heap tests" $ do
        prop "weight heap is balanced" (prop_balanced :: [Int] -> Bool)

weight_based_heap_tests = do
    heap_spec (T :: T (WeightBasedHeap Int))
    weight_based_heap_spec

