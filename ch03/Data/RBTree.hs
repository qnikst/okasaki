module Data.RBTree where

import Control.Arrow ((&&&))
import Data.List (foldl', sort, nub)
import Test.Hspec
import Test.Hspec.QuickCheck
import Debug.Trace

data Color  = R | B deriving Show
data Tree a =  E | T Color (Tree a) a (Tree a) deriving Show

-- Invariant1: no red node has a red child
-- Invariant2: everypath from the root to sn empty node containts 
-- the same number of black nodes

member :: (Ord a) => a -> Tree a ->  Bool
member v E = False
member v (T _ l x r) = 
    case v `compare` x of 
        LT -> v `member` l
        GT -> v `member` r
        EQ -> True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x t =
    let T _ a y b = go t
    in T B a y b
  where 
    go E = T R E x E
    go s@(T c a y b) = 
        case x `compare` y of
            LT -> balance c (go a) y b
            GT -> balance c a y (go b)
            EQ -> s
    balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
    balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
    balance c a x b = T c a x b

inv_no_red_red :: Tree a -> Bool
inv_no_red_red E = True
inv_no_red_red (T R (T R _ _ _) _ _) = False
inv_no_red_red (T R _ _ (T R _ _ _)) = False
inv_no_red_red (T _ l _ r) = inv_no_red_red l && inv_no_red_red r

inv_black :: (Ord a) => Tree a -> Bool
inv_black t = 
    case go t of
       [] -> True
       (x:xs) -> all (==x) xs
  where
    go E = [0]
    go (T R l _ r) = go l ++ go r
    go (T B l _ r) = map (+1) (go l++go r)

prop_all :: (Ord a) => [a] -> Bool
prop_all [] = True -- TODO: should fail
prop_all ls = toList (fromList ls) == nub (sort ls)
  where
    fromList = foldl' (flip insert) E 
    toList E = []
    toList (T _ l x r) = toList l ++ x:toList r

prop_no_red_red :: (Ord a) => [a] -> Bool
prop_no_red_red = inv_no_red_red . fromList
  where
    fromList = foldl' (flip insert) E 

prop_black :: (Ord a) => [a] -> Bool
prop_black = inv_black . fromList
  where
    fromList = foldl' (flip insert) E 

rbtree_test :: (Ord a) => Tree a -> Bool
rbtree_test = uncurry (&&) . (inv_no_red_red &&& inv_black)

rbtree_spec :: Spec
rbtree_spec = describe "is Red-Black tree" $ do
    prop "tree contains all elements" (prop_all :: [Int] -> Bool)
    prop "no red-red violations" (prop_no_red_red :: [Int] -> Bool)
    prop "no black violations" (prop_black :: [Int] -> Bool)
